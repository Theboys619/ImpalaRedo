using namespace Impala;

Value* fileReadImp(Value* self, std::vector<Value*> args, std::string file) {
  std::string fileName = args[0]->ToString();
  fs::path fullfile = fs::path(file).remove_filename() / fs::absolute(fs::path(fileName)).filename();
  
  return new Value(readFile(fullfile.string()));
}

Value* write(Value* self, std::vector<Value*> args, std::string file) {
  Value* outstream = args[0];
  Value* ended = outstream->Get("ended");

  while (!ended->ToBool() || !ended->ToInt()) {
    Value* x = ((Function*)outstream->Get("read"))->Call(std::vector<Value*>(), outstream, nullptr, false);
    ended = outstream->Get("ended");

    if (x->GetType() == ValueType::Nothing)
      break;

    std::cout << x->ToString();
  }

  return outstream;
}

void DefineGlobals(Interpreter* interp, Scope* globals) {

  // Reading Globals.imp file
  fs::path fullFile = fs::absolute(fs::path("./src/globals/globals.imp"));
  interp->Interpret(fullFile.string());

  // Creating Global Impala Object
  Value* Impala = new Value("[Object Impala]");
  Impala->Define("readFile", new Function(interp, fileReadImp));
  
  // Adding stdout to Impala Object
  Value* outstream = interp->ConstructClass("Stream", {}, interp->topScope);
  std::vector<Value*> args = { new Value("flush", "string"), new Function(interp, write) };
  ((Function*)outstream->Get("on"))->Call(args, outstream, interp->topScope, false);

  // Defining Impala and stdout Objects
  Impala->Define("stdout", outstream);
  globals->Define("Impala", Impala);
}