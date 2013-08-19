// Copyright 2013 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "class-examiner-extension.h"
#include "typing.h"
#include "parser.h"
#include "rewriter.h"
#include "stub-cache.h"

namespace v8 {
namespace internal {

const char* const ClassExaminerExtension::kSource =
    "native function classId();"
    "native function functionInfo();";


v8::Handle<v8::FunctionTemplate> ClassExaminerExtension::GetNativeFunction(
    v8::Handle<v8::String> str) {
  if (strcmp(*v8::String::Utf8Value(str), "classId") == 0) {
    return v8::FunctionTemplate::New(ClassExaminerExtension::GetClassId);
  } else {
    ASSERT(strcmp(*v8::String::Utf8Value(str), "functionInfo") == 0);
    return v8::FunctionTemplate::New(ClassExaminerExtension::GetFunctionInfo);
  }
}


void ClassExaminerExtension::GetClassId(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsObject()) {
    v8::ThrowException(v8::String::New(
        "First parameter to classId() must be an object."));
    return;
  }
  Handle<JSObject> object = Utils::OpenHandle(*args[0].As<v8::Object>());
  Map* map = object->map();
  args.GetReturnValue().Set(
        static_cast<uint32_t>(reinterpret_cast<uintptr_t>(map)));
}


inline static void Write(Handle<SeqTwoByteString> out,
                         int& index,
                         const char* string) {
  while (*string)
    out->SeqTwoByteStringSet(index++, static_cast<uint16_t>(*string++));
}


inline static void Write(Handle<SeqTwoByteString> out,
                         int& index,
                         char chr) {
  out->SeqTwoByteStringSet(index++, static_cast<uint16_t>(chr));
}


template <typename Char>
static INLINE(Vector<const Char> GetCharVector(String* string));


template <>
Vector<const uint8_t> GetCharVector(String* string) {
  String::FlatContent flat = string->GetFlatContent();
  ASSERT(flat.IsAscii());
  return flat.ToOneByteVector();
}


template <>
Vector<const uc16> GetCharVector(String* string) {
  String::FlatContent flat = string->GetFlatContent();
  ASSERT(flat.IsTwoByte());
  return flat.ToUC16Vector();
}


inline static void Write(Handle<SeqTwoByteString> out,
                         int& index,
                         String* string) {
  ASSERT(string->IsFlat());
  int length = string->length();
  if (string->IsOneByteRepresentationUnderneath()) {
    Vector<const uint8_t> vector = GetCharVector<uint8_t>(string);
    for (int i = 0; i < length; i++)
      out->SeqTwoByteStringSet(index++, vector[i]);
  } else {
    Vector<const uint16_t> vector = GetCharVector<uint16_t>(string);
    for (int i = 0; i < length; i++)
      out->SeqTwoByteStringSet(index++, vector[i]);
  }
}


#define RECURSE(call)                      \
  do {                                     \
    ASSERT(!this->HasStackOverflow());     \
    call;                                  \
    if (this->HasStackOverflow()) return;  \
  } while (false)


class ICOutputer: public AstTyper {
 public:
  void* operator new(size_t size, Zone* zone) {
    return zone->New(static_cast<int>(size));
  }
  explicit ICOutputer(CompilationInfo* info,
                      Handle<SeqTwoByteString> result,
                      int* index)
    : AstTyper(info), result_(result), index_(index) {
    if (!Parser::Parse(info) || !Rewriter::Rewrite(info) ||
        !Scope::Analyze(info))
      UNREACHABLE(); // could this ever happen?
    RECURSE(this->VisitStatements(info->function()->body()));
  }
  void Output(Handle<SeqTwoByteString> out, int* index) {
    result_ = out;
    index_ = index;
    RECURSE(this->VisitStatements(info_->function()->body()));
  }

 private:
  inline void Write(const char* string) {
    if (result_.is_null()) {
      *index_ += strlen(string);
    } else {
      while (*string)
        result_->SeqTwoByteStringSet((*index_)++,
                                     static_cast<uint16_t>(*string++));
    }
  }
  inline void Write(String* string) {
    // Don't create handles during the dry run.
    int length = string->length();
    if (result_.is_null()) {
      *index_ += length;
      return;
    }

    Handle<String> flattened;
    {
      AllowHeapAllocation some_allocation;
      flattened = FlattenGetString(Handle<String>(string, info_->isolate()));
    }
    ASSERT(flattened->IsFlat());

    if (flattened->IsOneByteRepresentationUnderneath()) {
      Vector<const uint8_t> vector = GetCharVector<uint8_t>(*flattened);
      for (int i = 0; i < length; i++)
        result_->SeqTwoByteStringSet((*index_)++, vector[i]);
    } else {
      Vector<const uint16_t> vector = GetCharVector<uint16_t>(*flattened);
      for (int i = 0; i < length; i++)
        result_->SeqTwoByteStringSet((*index_)++, vector[i]);
    }
  }
  inline void Write(int num) {
    char int_out[std::numeric_limits<int>::digits + 1];
    snprintf(int_out, std::numeric_limits<int>::digits, "%d", num);
    Write(int_out);
  }
  // This function outputs things like (A x 1, B x 2, ...) in the order of the
  // the list. When count_dict is true, it is followed by "x of them are in
  // dictionary mode". When output_length is true, it is preceded by "x hidden
  // classes cached".
  void OutputMapList(SmallMapList* list, bool output_length, bool count_dict) {
    int length = list->length();
    if (output_length) {
      if (!length) {
        // For Load_IC, this is probably becsue previous hidden classes are
        // deprecated. For Call_IC, this might be because the IC just became
        // MEGAMORPHIC so there is nothing in the megamorphic cache. In any
        // case, this is too complicated to explain. So, we just say
        Write("no hidden class cached");
        return;
      }
      else {
        Write(length);
        Write(" hidden classes cached ");
      }
    }

    SmallMapList dictionary_maps;
    Write("(");
    ZoneList<Handle<Map> > copied_list(length, info_->zone());
    for (int i = 0; i < length; i++)
      copied_list.Add(list->at(i), info_->zone());

    bool is_first = true;
    while (!copied_list.is_empty()) {
      Handle<Map> map_to_count = copied_list.first();
      Object* fun = map_to_count->constructor();
      int count = 0;
      for (int i = copied_list.length() - 1; i > -1; i--) {
        if (copied_list[i]->constructor() == fun) {
          if (copied_list[i]->is_dictionary_map())
            dictionary_maps.Add(copied_list[i], info_->zone());
          copied_list.Remove(i);
          count++;
        }
      }

      if (!is_first)
        Write(", ");
      else
        is_first = false;

      if (!fun->IsNull()) {
        JSFunction* js_fun = JSFunction::cast(fun);
        Handle<Object> displayName =
          GetProperty(Handle<JSFunction>(js_fun, info_->isolate()),
                      "displayName");
        Object* fun_name = js_fun->shared()->name();
        // Use "displayName" if it's available as a String*.
        if (displayName->IsString()) {
          Write(*Handle<String>::cast(displayName));
        } else if (fun_name->IsString() && String::cast(fun_name)->length()) {
          Write(String::cast(fun_name));
        } else {
          Write("<");
          String* inferred_name = js_fun->shared()->inferred_name();
          if (inferred_name->length())
            Write(inferred_name);
          else
            Write("unknown");
          Write(">");
        }
        Write(" x ");
        Write(count);
      } else {
        Write("<map for builtin objects> x ");
        Write(count);
      }
    }
    Write(")");

    // For LOAD_IC/STORE_IC, the type oracle don't collect dictionary maps so
    // we don't do this here. On the other hand, they have extra check for
    // dictionary access. See below
    if (!count_dict) return;
    if (dictionary_maps.is_empty()) {
      Write(" and they are all in fast mode!");
    } else {
      Write(" and ");
      Write(dictionary_maps.length());
      Write(" of them are in dictioanry mode ");
      OutputMapList(&dictionary_maps, false, false);
      Write(" :(");
    }
  }
  // For MEGAMORPHIC ic, this function guesses whether there have been
  // dictioanry objects passed by. This is inaccurate for three reasons: 1)
  // megamorphic cache is shared between different MEGAMORPHIC callsites so the
  // "passed through" is just... a guess 2) when a kLoadIC_Normal turns into
  // MEGAMORPHIC, that kLoadIC_Normal doesn't have an associated Handle<Name>
  // so it can't be copied to the megamorphic cache. 3) hash collision :(
  void OutputPassedByDictionaryMaps(Handle<String> name, Code::Kind kind) {
    Code::Flags flags =
      Code::ComputeFlags(kind, MONOMORPHIC, Code::kNoExtraICState);
    SmallMapList potential_dictionary_maps;
    info_->isolate()->stub_cache()->CollectMatchingMaps(
       &potential_dictionary_maps, name, flags,
       Handle<Context>(info_->closure()->context()->native_context(),
       info_->isolate()), info_->zone());
    int length = potential_dictionary_maps.length();

    // Sometimes CollectMatchingMaps would match Code*'s with a different flag
    // , say, STUB, becasue of hash collision and the corresponding Map* would
    // not be a dictionary map. We remove those maps from the list to workaround
    // this situation.
    SmallMapList dictionary_maps(length, info_->zone());
    for (int i = 0; i < length; i++) {
      if (potential_dictionary_maps.at(i)->is_dictionary_map())
        dictionary_maps.Add(potential_dictionary_maps.at(i),
                            info_->zone());
    }
    if (!dictionary_maps.is_empty()) {
      Write(" with some dictionary objects passed through ");
      OutputMapList(&dictionary_maps, false, false);
      Write(" :(");
    }
    else {
      Write(" with no dictionary objects passed through!");
    }
  }
  virtual void VisitAssignment(Assignment* assignment) {
    AstTyper::VisitAssignment(assignment);

    Property* prop = assignment->target()->AsProperty();
    // TODO: Why doesn't AstTyper::VisitAssignmentfetch StoreIC info of a
    // compund assignment ('+=', '-=') when it's not keyed?
    if (!assignment->is_compound() && prop && prop->key()->IsPropertyName()) {
      Literal* lit_key = prop->key()->AsLiteral();
      ASSERT(lit_key != NULL && lit_key->value()->IsString());
      Handle<String> name = Handle<String>::cast(lit_key->value());
      Handle<Object> object =
        oracle()->GetInfo(assignment->AssignmentFeedbackId());

      Write( "* [[PUT]]('");
      Write(*name);
      Write("') : ");
      bool is_code = object->IsCode();
      Code* code = is_code ? Code::cast(*object) : NULL;
      Builtins* builtins = info_->isolate()->builtins();
      if (code == builtins->builtin(Builtins::kStoreIC_Initialize) ||
          code == builtins->builtin(Builtins::kStoreIC_Initialize_Strict)) {
        Write("no hidden class cached (not run)");
      } else if
         // StoreIC has no PREMONOMORPHIC state.
         (code == builtins->builtin(Builtins::kStoreIC_Normal) ||
          code == builtins->builtin(Builtins::kStoreIC_Normal_Strict)) {
        Write("no hidden class cached (dictionary mode access)");
      } else if
         (code == builtins->builtin(Builtins::kStoreIC_Generic) ||
          code == builtins->builtin(Builtins::kStoreIC_Generic_Strict)) {
        Write("no hidden class cached (not cacheable)");
      } else {
        OutputMapList(assignment->GetReceiverTypes(), true, false);
        if
         (code == builtins->builtin(Builtins::kStoreIC_Megamorphic) ||
          code == builtins->builtin(Builtins::kStoreIC_Megamorphic_Strict))
          OutputPassedByDictionaryMaps(name, Code::STORE_IC);
        else
          Write(" with no dictionary objects passed through!");
       }
      Write("\n");
    }
  }
  virtual void VisitProperty(Property* prop) {
    AstTyper::VisitProperty(prop);

    Handle<Object> object = oracle()->GetInfo(prop->PropertyFeedbackId());
    // VistAssignment/Call would call VisitProperty but they don't actually
    // have IC associed. This is too confusing to display so we ignore its.
    if (object->IsUndefined()) return;

    // Don't dump keyed load yet.
    if (prop->key()->IsPropertyName()) {
      Literal* lit_key = prop->key()->AsLiteral();
      // TODO: check protototype/length
      ASSERT(lit_key != NULL && lit_key->value()->IsString());
      Handle<String> name = Handle<String>::cast(lit_key->value());

      Write("* [[GET]]('");
      Write(*name);
      Write("') : ");
      bool is_code = object->IsCode();
      Code* code = is_code ? Code::cast(*object) : NULL;
      Builtins* builtins = info_->isolate()->builtins();
      if (code == builtins->builtin(Builtins::kLoadIC_Initialize)) {
        Write("no hidden class cached (not run)");
      } else if (code == builtins->builtin(Builtins::kLoadIC_PreMonomorphic)) {
        Write("no hidden class cached (run only once)");
      } else if (code == builtins->builtin(Builtins::kLoadIC_Normal)) {
        Write("no hidden class cached (dictionary mode access)");
      } else if (code == builtins->builtin(Builtins::kLoadIC_Slow)) {
        Write("no hidden class cached (not cacheable)");
      }
      else {
        OutputMapList(prop->GetReceiverTypes(), true, false);
        if (code == builtins->builtin(Builtins::kLoadIC_Megamorphic))
          OutputPassedByDictionaryMaps(name, Code::LOAD_IC);
        else
          Write(" with no dictionary objects passed through!");
      }
      Write("\n");
    }
  }
  virtual void VisitCall(Call* call) {
    AstTyper::VisitCall(call);

    Expression* callee = call->expression();
    Property* prop = callee->AsProperty();
    if (prop && prop->key()->IsPropertyName()) {
      Literal* key = prop->key()->AsLiteral();
      ASSERT(key != NULL && key->value()->IsString());
      Handle<String> name = Handle<String>::cast(key->value());
      Handle<Object> object = oracle()->GetInfo(call->CallFeedbackId());

      Write("* [[GET]]('");
      Write(*name);
      Write("') + [[CALL]] : ");
      bool is_code = object->IsCode();
      if (is_code && Code::cast(*object)->ic_state() == UNINITIALIZED)
        Write("no hidden class cached (not run)");
      else if (is_code && Code::cast(*object)->ic_state() == PREMONOMORPHIC)
        Write("no hidden class cached (run only once)");
      else
        OutputMapList(call->GetReceiverTypes(), true, true);
      Write("\n");
    }
  }
  // TODO: Do something about global load?

  Handle<SeqTwoByteString> result_;
  int* index_;
};

void ClassExaminerExtension::GetFunctionInfo(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsFunction()) {
    v8::ThrowException(v8::String::New(
        "First parameter to functionInfo() must be a function."));
    return;
  }

  // Calculate the length of the output.
  Handle<JSFunction> fun = Utils::OpenHandle(*args[0].As<v8::Function>());
  int fun_info_len = 0;

  // Step 1: function header
  Handle<Object> displayName = GetProperty(fun, "displayName");
  Object* fun_name = fun->shared()->name();
  // Use "displayName" if it's available as a String*.
  if (displayName->IsString()) {
    fun_info_len += 10; // "Function: %s"
    fun_info_len += Handle<String>::cast(displayName)->length();
  } else if (fun_name->IsString() && String::cast(fun_name)->length()) {
    fun_info_len += 10; // "Function: %s"
    fun_info_len += String::cast(fun_name)->length();
  } else {
    fun_info_len += 22; // "Anonymous function: <%s>"
    fun_info_len += fun->shared()->inferred_name()->length();
  }
  fun_info_len += 1; // "\n"

  // Step 2: instructions with IC
  CompilationInfoWithZone* info = NULL;
  ICOutputer *outputer = NULL;
  // 'functinInfo' isn't fun->IsBuiltin() nor fun->shared()->native()
  // and I don't know why. In any case, having undefined script() would
  // trigger an asertion when the CompilationInfo is created so we use that
  // as the condition here.
  if (!fun->shared()->HasSourceCode()) {
    fun_info_len += 10; // "(Builtin)\n"
  // As long as the shared info is compiled, it doesn't matter if the Code*
  // the function ifself holds is Crankshafted or not becuase AstTyper
  // analyzes the Code* on the shared info and that's always full-gen Code*.
  } else if (fun->shared()->is_compiled()) {
    info = new CompilationInfoWithZone(fun);
    // This is a dry that doesn't write the string but builds the AST and
    // increases fun_info_len.
    outputer = new(info->zone()) ICOutputer(info, Handle<SeqTwoByteString>(),
                                            &fun_info_len);
  } else {
    fun_info_len += 68; // "(This function is not compiled so there's no
                        //   information available)\n"
  }

  // Write the output into the result string.
  Isolate* isolate = fun->GetIsolate();
  Handle<SeqTwoByteString> result =
        isolate->factory()->NewRawTwoByteString(fun_info_len);
  int index = 0;
  DisallowHeapAllocation no_allocation;

  // Step 1: function header
  if (displayName->IsString()) {
    Write(result, index, "Function: ");
    Write(result, index, String::cast(*displayName));
  } else if (fun_name->IsString() && String::cast(fun_name)->length()) {
    Write(result, index, "Function: ");
    Write(result, index, String::cast(fun_name));
  } else {
    Handle<String> inferred_name;
    {
      AllowHeapAllocation some_allocation;
      inferred_name =
        FlattenGetString(Handle<String>(fun->shared()->inferred_name()));
    }
    Write(result, index, "Anonymous function: <");
    Write(result, index, *inferred_name);
    Write(result, index, '>');
  }
  Write(result, index, '\n');

  // Step 2: instructions with IC
  if (!fun->shared()->HasSourceCode()) {
    Write(result, index, "(Builtin)\n");
  } else if (fun->is_compiled()) {
    outputer->Output(result, &index);
    delete info; // This deletes the outputer too.
  } else {
    Write(result, index, "(This function is not compiled so there's no"
                         " information available)\n");
  }

  Handle<String> casted_result = Handle<String>::cast(result);
  args.GetReturnValue().Set(Utils::ToLocal(casted_result));
}


void ClassExaminerExtension::Register() {
  static ClassExaminerExtension class_examiner_extension;
  static v8::DeclareExtension declaration(&class_examiner_extension);
}

} }  // namespace v8::internal
