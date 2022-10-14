<!-- start server like this:

php -S localhost:8003

where you can replace 8003 with any available port you want
obviously index.php must be in the same directory -->

<?php

$problems = array(
  "1" => "·–·· ––– ·–· · ––  ·· ·––· ··· ··– ––  –·· ––– ·–·· ––– ·–·  ··· ·· –  ·– –– · –",
  "2" => "–·–· ––– –· ··· · –·–· – · – ··– ·–·  ·– –·· ·· ·––· ·· ··· ·· –·–· ·· –· ––·  · ·–·· ·· –",
  "3" => "··· · –··  –·· –––  · ·· ··– ··· –– ––– –··  – · –– ·––· ––– ·–·  ·· –· –·–· ·· –·· ·· –·· ··– –· –  ··– –  ·–·· ·– –··· ––– ·–· ·  · –  –·· ––– ·–·· ––– ·–· ·  –– ·– ––· –· ·–  ·– ·–·· ·· ––·– ··– ·–  ··· – ––– ·––·",
  "4" => "··– –  · –· ·· ––  ·– –··  –– ·· –· ·· ––  ···– · –· ·· ·– ––",
  "5" => "––·– ··– ·· ···  –· ––– ··· – ·–· ··– –··  · –··– · ·–· –·–· ·· – ·– – ·· ––– –·  ··– ·–·· ·–·· ·– –– –·–· –––  ·–·· ·– –··· ––– ·–· ·· ···  –· ·· ··· ··  ··– –  ·– ·–·· ·· ––·– ··– ·· ·––·  · –··–  · ·–  –·–· ––– –– –– ––– –·· –––  –·–· ––– –· ··· · ––·– ··– ·– –  ··· – ––– ·––·"
);

$answers = array(
  "1" => "lorem ipsum dolor sit amet",
  "2" => "consectetur adipisicing elit",
  "3" => "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua stop",
  "4" => "ut enim ad minim veniam",
  "5" => "quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat stop"
);

session_start();


if(!isset($_SESSION["stage"])){
  $_SESSION["stage"] = "1";
  $stage = "1"; 
}
else
  $stage = $_SESSION["stage"];
if(!isset($_SESSION["state"])){
  $_SESSION["state"] = "na";
  $state = "na";
}
else
  $state = $_SESSION["state"];



$answer_given = false;
$right_answer = false;
if(isset($_POST["answer"])){
  $answer_given = true;

  if(strcasecmp(trim($_POST["answer"]), $answers[$stage]) == 0){
    $right_answer = true;
  }
} else if ($state == "a"){
  $answer_given = false;
  $state = "na";
}

$continue=false;
if(isset($_POST["continue"]))
  $continue = true;

if(!$answer_given && $state == "na")
  $show_html = "A";
else if ($answer_given && $right_answer) {
  $show_html = "B";  
  $_SESSION["stage"] = strval(((int)$stage)+1);
  $_SESSION["state"] = "a";
}
else if($answer_given && !$right_answer) {
  $show_html = "C";
  $_SESSION["state"] = "a";
  $_SESSION["stage"] = $stage;
};

if($continue){
  $show_html = "A";
  $_SESSION["state"] = "na";
};



if((int)$stage > 5 ) {
  $show_html = "E";
  $_SESSION["state"] = "na";
  $_SESSION["stage"] = "1";

} 

?>


<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Yet another simple code game!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.title {
  font-family: "Courier New", Courier, monospace;
  font-weight: bold;
  font-size: 48px;
  color: #00FF66;
}

.question {color: #FFCC33}
.number {color: #FFFF33}
.md5sum {color: #FFCCFF}
.emph {color: #99ee99}
.alert {color: #ee77aa}

.right {
  color: #33FF66;
  font-weight: bold;
}
.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link {
  color: #CCFFFF;
}

a:visited {
  color: #CCFFFF;
}

input {
  background-color: #eeee66;
  color: #333399;
}

code {
  text-wrap: lowercase;   
  font-family: monospace;
  display: block;
  background-color: #66eeee;
  color: #993333;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
  margin-bottom: 2em;
}

textarea.wide {
  text-wrap: lowercase;
  font-family: monospace;
  font-size: x-large;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
}

-->
</style>
</head>
<body>


<h1>Yet another simple code game!</h1>

<p><span class="question"> <?php if($show_html == "E") echo "DONE"; else echo ("Question ".$stage);?> 
</span>:</p>

<code><?php if($show_html == "E") echo "CONGRATULATIONS!"; else echo $problems[$stage];?></code>


<?php if($show_html == "A"){ ?> 
<span class="question">Make it quick, the clock is ticking... </span></p>
<form action="index.php" id="f" name="f" method="post">
  <textarea class="wide" name="answer" id="answer"></textarea><br />
  <input type="submit" name="submit" id="submit" value="Submit!" />
  <input type="reset" value="Reset" />
</form>
<?php }else if($show_html == "B"){ ?>
  <p class="right">Right!  :-)</p>
<hr />
<form action="index.php" id="r" name="r" method="post">
<input type="hidden" id="continue" name="continue" value="continue" />
<input type="submit" name="again" id="again" value="Continue!" />
</form>

<?php } else if($show_html == "C") { ?>

<p class="wrong">Wrong!  :-(</p>
<hr />
<form action="index.php" id="r" name="r" method="post">
<input type="hidden" id="continue" name="continue" value="continue" />
<input type="submit" name="again" id="again" value="Continue!" />
</form>

<?php } else if ($show_html == "E") {?>
  <form action="index.php" id="r" name="r" method="post">
<input type="hidden" id="reset" name="reset" value="reset" />
<input type="submit" name="again" id="again" value="Play Again!" />
<?php }?>

</body>
</html>