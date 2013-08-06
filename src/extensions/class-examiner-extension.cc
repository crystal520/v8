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
    ASSERT(string->IsFlat());
    int length = string->length();

    if (result_.is_null()) {
      *index_ += length;
      return;
    }

    if (string->IsOneByteRepresentationUnderneath()) {
      Vector<const uint8_t> vector = GetCharVector<uint8_t>(string);
      for (int i = 0; i < length; i++)
        result_->SeqTwoByteStringSet((*index_)++, vector[i]);
    } else {
      Vector<const uint16_t> vector = GetCharVector<uint16_t>(string);
      for (int i = 0; i < length; i++)
        result_->SeqTwoByteStringSet((*index_)++, vector[i]);
    }
  }
  inline void Write(int num) {
    char int_out[std::numeric_limits<int>::digits + 1];
    snprintf(int_out, std::numeric_limits<int>::digits, "%d", num);
    Write(int_out);
  }
  void OutputMapList(SmallMapList* list) {
    int length = list->length();
    if (!length) {
      Write("nothing cached (not run or not cacheable)");
      return;
    }

    // int dictionary_map_count = 0;
    Write(length);
    Write(" hidden classes cached (");

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
          // if (copied_list[i]->is_dictionary_map()) dictionary_map_count++;
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
        Object* fun_name = js_fun->shared()->name();
        if (fun_name->IsString() && String::cast(fun_name)->length()) {
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

    // We don't do this for now because it seems that the maps collected
    // would never be in dictionary mode.

    /*if (!dictionary_map_count) {
      Write(" and they are all in fast mode!");
    } else {
      Write(" and ");
      Write(dictionary_map_count);
      Write(" of them are in dictioanry mode :(");
    }*/
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

      Write( "* [[PUT]]('");
      Write(*name);
      Write("') : ");
      OutputMapList(assignment->GetReceiverTypes());
      Write("\n");
    }
  }
  virtual void VisitProperty(Property* prop) {
    AstTyper::VisitProperty(prop);

    // VistAssignment/Call would call VisitProperty but they don't actually
    // have IC associed. This is too confusing to display so we ignore its.
    if (oracle()->GetInfo(prop->PropertyFeedbackId())->IsUndefined()) return;

    // Don't dump keyed load yet.
    if (prop->key()->IsPropertyName()) {
      Literal* lit_key = prop->key()->AsLiteral();
      // TODO: check protototype/length
      ASSERT(lit_key != NULL && lit_key->value()->IsString());
      Handle<String> name = Handle<String>::cast(lit_key->value());

      Write("* [[GET]]('");
      Write(*name);
      Write("') : ");
      OutputMapList(prop->GetReceiverTypes());
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

      Write("* [[GET]]('");
      Write(*name);
      Write("') + [[CALL]] : ");
      OutputMapList(call->GetReceiverTypes());
      Write("\n");
    }
  }

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
  Object* fun_name = fun->shared()->name();
  if (fun_name->IsString() && String::cast(fun_name)->length()) {
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
  if (!fun->shared()->HasSourceCode()) {
    // functinInfo isn't fun->IsBuiltin() nor fun->shared()->native()
    // and I don't know why. In any case, having undefined script() would
    // trigger an asertion when the CompilationInfo is created.
    fun_info_len += 10; // "(Builtin)\n"
  } else if (fun->is_compiled()) {
    info = new CompilationInfoWithZone(fun);
    // This does a dry run of the AST traversal and only increases fun_info_len.
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
  if (fun_name->IsString() && String::cast(fun_name)->length()) {
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
