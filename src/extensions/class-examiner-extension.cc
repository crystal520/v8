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


void ClassExaminerExtension::GetFunctionInfo(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsFunction()) {
    v8::ThrowException(v8::String::New(
        "First parameter to functionInfo() must be a function."));
    return;
  }
  Handle<JSObject> object = Utils::OpenHandle(*args[0].As<v8::Object>());
  Map* map = object->map();
  args.GetReturnValue().Set(
        static_cast<uint32_t>(reinterpret_cast<uintptr_t>(map)));
}


void ClassExaminerExtension::Register() {
  static ClassExaminerExtension class_examiner_extension;
  static v8::DeclareExtension declaration(&class_examiner_extension);
}

} }  // namespace v8::internal
