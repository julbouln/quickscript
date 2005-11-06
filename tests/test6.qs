package {

class bla {
  $truc="blabla"
    };

class chose {
$v="chose";
};

class test {
  inherit bla;  
  $a=10;

  function b() {
    print($a);
    print($truc);
  };

  #c=new chose;
};

}
