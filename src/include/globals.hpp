using namespace Impala;

Value* fileReadImp(std::vector<Value*> args, std::string file, Interpreter* interp) {
  std::string fileName = args[0]->ToString();
  fs::path fullfile = fs::path(file).remove_filename() / fs::path(fileName);

  return new Value(readFile(fullfile.string()));
}

Value* write(std::vector<Value*> args, std::string file, Interpreter* interp) {
  Value* stdout = args[0];
  Value* ended = stdout->Get("ended");

  while (!ended->ToBool() || !ended->ToInt()) {
    Value* x = ((Function*)stdout->Get("read"))->Call(std::vector<Value*>(), stdout, interp->currentScope, false);
    ended = stdout->Get("ended");

    if (x->GetType() == ValueType::Nothing)
      break;

    std::cout << x->ToString();
  }

  return stdout;
}

void DefineGlobals(Interpreter* interp, Scope* globals) {
  fs::path fullFile = fs::current_path() / fs::path("./src/globals/globals.imp");
  // std::cout << fullFile.string() << std::endl;

  interp->Interpret(fullFile.string());

  Value* Impala = new Value("[Object Impala]");
  Impala->Define("readFile", new Function(interp, fileReadImp));
  
  Value* stdout = interp->ConstructClass("Stream", {}, interp->topScope);
  std::vector<Value*> args = { new Value("flush", "string"), new Function(interp, write) };

  ((Function*)stdout->Get("on"))->Call(args, stdout, interp->topScope, false);

  Impala->Define("stdout", stdout);
  
  globals->Define("Impala", Impala);
}