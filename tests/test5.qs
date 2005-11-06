package {
$d=(10+4)*5;
$e="bla"."truc"."chouette";
print($d);
print($e);
function test($a,$b) {
print("bla"."truc");
if ($a>10) {
print("if");
print($a);
};
print($b);
};
//print("bidule");
test(15,20);
//print("truc");
}