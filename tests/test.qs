package {
$a=1;
$b="bla";
print($a);
function bla () { 
print("truc");
print("bidule");
};
$g=concat($b,$b);
if($a<20) {
print("if");
print($g)
} else {
print(0)
};
$i=0;
while ($i<10) {
print($i);
$i=$i+1
};
$r=random(100);
print($r);
bla();
}