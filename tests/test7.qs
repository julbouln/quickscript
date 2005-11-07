package {
load("./test_lib.so");
include("tests/test6.qs");
print_string2("blabla");
#t1=new test;
print(#t1->$a);
#t2=#t1->#c;
print(#t2->$v);
print(#t1->#c->$v);
@l=(1,"bla","bidule");
$k=@l.(0);
print($k);
print(@l.(1));

}