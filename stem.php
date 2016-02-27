<?php

// stup sastrawi
// include composer autoloader
require_once "C:/php/composer/vendor/autoload.php";

// create stemmer
// cukup dijalankan sekali saja, biasanya didaftarkan di service container
$stemmerFactory = new \Sastrawi\Stemmer\StemmerFactory();
$stemmer  = $stemmerFactory->createStemmer();

// file handler
$fr = fopen("D:/research/TWEET/df_stemming-source.csv","r");
$fw = fopen("D:/research/TWEET/df_stemming-result.csv","w");

// 1st line (title)
$title = fgetcsv($fr);
array_push($title, "stemming");
fputcsv($fw, $title);

$n = 0;
while(!feof($fr)) {
	$row = fgetcsv($fr);	
	$sentence = $row[1];
	$stem = $stemmer->stem($sentence);
	array_push($row, $stem);
	fputcsv($fw, $row);
	
	$n++;
	
	if(!array_key_exists(1, $row)){
		print("\n WARNING! :: Problematic tweet : $n\n");
	}
	
	// every 1000 iters
	if($n % 10000 == 0){
		flush();	// flush write to file
		// print("\n$n\norig : $sentence\nstem : $stem\n");
		print("$n ");
	}	
	
}

print("\n >> FINISHED. Tweet processed : $n");	

// close file
fclose($fr);
fclose($fw);
?>