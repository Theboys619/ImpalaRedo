using namespace Impala;

Value* fileReadImp(Value* self, std::vector<Value*> args, std::string file) {
  std::string fileName = args[0]->ToString();
  fs::path fullfile = fs::path(file).remove_filename() / fs::absolute(fs::path(fileName)).filename();
  
  return new Value(readFile(fullfile.string()));
}

Value* write(Value* self, std::vector<Value*> args, std::string file) {
  Value* stdout = args[0];
  Value* ended = stdout->Get("ended");

  while (!ended->ToBool() || !ended->ToInt()) {
    Value* x = ((Function*)stdout->Get("read"))->Call(std::vector<Value*>(), stdout, nullptr, false);
    ended = stdout->Get("ended");

    if (x->GetType() == ValueType::Nothing)
      break;

    std::cout << x->ToString();
  }

  return stdout;
}

void DefineGlobals(Interpreter* interp, Scope* globals) {

  // Reading Globals.imp file
  fs::path fullFile = fs::absolute(fs::path("./src/globals/globals.imp"));
  interp->Interpret(fullFile.string());

  // Creating Global Impala Object
  Value* Impala = new Value("[Object Impala]");
  Impala->Define("readFile", new Function(interp, fileReadImp));
  
  // Adding stdout to Impala Object
  Value* stdout = interp->ConstructClass("Stream", {}, interp->topScope);
  std::vector<Value*> args = { new Value("flush", "string"), new Function(interp, write) };
  ((Function*)stdout->Get("on"))->Call(args, stdout, interp->topScope, false);

  // Defining Impala and stdout Objects
  Impala->Define("stdout", stdout);
  globals->Define("Impala", Impala);
}