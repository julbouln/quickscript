package {
include("tests/test6.qs");
#t1=new test;
print(#t1->$a);
#t2=#t1->#c;
print(#t2->$v);
print(#t1->#c->$v);
//%l=(1,"bla","bidule");
//$k=%l[0];
//print(%l[0]);
}