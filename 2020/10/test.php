#!/usr/bin/php 

<?php

$alphabet = array(
    "·–" => "a",
    "–···" => "b",
    "–·–·" => "c",
    "–··" => "d",
    "·" => "e",
    "··–·" => "f",
    "––·" => "g",
    "····" => "h",
    "··" => "i",
    "·–––" => "j",
    "–·–" => "k",
    "·–··" => "l",
    "––" => "m",
    "–·" => "n",
    "–––" => "o",
    "·––·" => "p",
    "––·–" => "q",
    "·–·" => "r",
    "···" => "s",
    "–" => "t",
    "··–" => "u",
    "···–" => "v",
    "·––" => "w",
    "–··–" => "x",
    "–·––" => "y",
    "––··" => "z"
);

$url = argv[1];


$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HEADER, true);

$data = curl_exec($ch);
curl_close($ch);


preg_match_all('/^Set-Cookie:\s*([^\r\n]*)/mi', $data, $ms);

$cookies = array();
foreach ($ms[1] as $m) {
    list($name, $value) = explode('=', $m, 2);
    $cookies[$name] = $value;
}

$mpiskoto = strtok($cookies['PHPSESSID'], ";");


do{

$dom = new domDocument;
@$dom->loadHtml($data);

$morse = $dom->getElementsByTagName('code');

preg_match("/<code>.*<\/code>/", 
                     $data,
                     $morse);

$morse[0] = str_replace("<code>", "", $morse[0]);
$morse[0] = str_replace("</code>", "", $morse[0]);

$words = explode("  ", $morse[0]);

$answer = "";
foreach ($words as $word) {
    $letters = explode(" ", $word);
    if(!array_key_exists($letters[0], $alphabet))
    {
        exit();
    }
    foreach($letters as $letter) {
        echo $alphabet[$letter];
        $answer = $answer . $alphabet[$letter];
    }
    echo " ";        
    $answer = $answer . " ";

}

$fields = [
    'answer' => $answer,
];

$postdata = http_build_query($fields);

$curl = curl_init($url);
curl_setopt($curl,CURLOPT_POST, true);
curl_setopt($curl,CURLOPT_POSTFIELDS, $postdata);
curl_setopt($curl,CURLOPT_RETURNTRANSFER, true);
curl_setopt($curl, CURLOPT_HTTPHEADER, array("Cookie: PHPSESSID=".$mpiskoto));

$result = curl_exec($curl);


echo "\n";


$fields = [
    'continue'=> 'true'
];

$postdata = http_build_query($fields);

curl_setopt($curl,CURLOPT_POSTFIELDS, $postdata);
$data = curl_exec($curl);
echo "\n";
curl_close($curl);

} while(true);
?>