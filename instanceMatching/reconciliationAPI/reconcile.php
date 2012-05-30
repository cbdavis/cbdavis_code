<?php
header("Content-type: application/json; charset=utf-8");
//header("Content-type: application/json");

setlocale(LC_CTYPE, "en_US.UTF-8");

#this works for testing
#http://localhost/testMatch.php?query={%22query%22:%22Awirs%22,%22limit%22:3,%22type%22:%22Powerplant%22,%22type_strict%22:%22any%22,%22properties%22:[{%22p%22:%22Country%22,%22v%22:%22Belgium%22}]}

#used for Google Refine - Add column by fetching URLs based on column Name
#'http://localhost/testMatch.php?query={%22query%22:%22'+escape(cells['Name'].value, 'url')+'%22,%22limit%22:3,%22type%22:%22Powerplant%22,%22type_strict%22:%22any%22,%22properties%22:[{%22p%22:%22Country%22,%22v%22:%22'+cells['Country'].value+'%22}]}'

#$query = $_GET['query'];
$query = isset($_GET['query']) ? $_GET['query'] : "";

#expect to get callback=jsonp
#then the reconciliation service returns info about itself
#$callback = $_GET['callback'];
$callback = isset($_GET['callback']) ? $_GET['callback'] : "";

if (isset($_POST) && $callback == "") { //multiple queries
	file_put_contents('php://stderr', 'Post Contents - ');
	file_put_contents('php://stderr', print_r($_POST, true));

        //TODO allow for graceful failure
        if (isset($_POST['queries'])){
                $RCommand = "Rscript ReconciliationAPI.R ".escapeshellarg($_POST['queries']);
        } else {
                $RCommand = "Rscript ReconciliationAPI.R ".escapeshellarg($_POST['query']);
        }

	file_put_contents('php://stderr', 'encoding is '.mb_detect_encoding($_POST['queries']));

	file_put_contents('php://stderr', 'sending R command - ');
	file_put_contents('php://stderr', $RCommand);

	$output = shell_exec($RCommand);

	print_r($output);
	file_put_contents('php://stderr', 'Response - ');
	file_put_contents('php://stderr', print_r($output, true));

} else if ($callback != ""){
	#!!! Make sure $callback is included, otherwise it won't work
	$callbackResponse = $callback."({
					  \"name\" : \"Enipedia Power Plant Reconciliation\",
					  \"identifierSpace\" : \"http://rdf.freebase.com/ns/type.object.mid\",
					  \"schemaSpace\" : \"http://rdf.freebase.com/ns/type.object.id\",
					  \"view\" : {
					    \"url\" : \"http://www.freebase.com/view{{id}}\"
					  },
					  \"preview\" : {
					    \"url\" : \"http://www.freebase.com/widget/topic{{id}}?mode=content\",
					    \"width\" : 430,
					    \"height\" : 300
					  },
					  \"suggest\" : {
					    \"property\" : \"http://localhost/testMatch.php\"
					  },
					  \"defaultTypes\" : [
					    {
					      \"id\" : \"/Category:Powerplant\",
					      \"name\" : \"Powerplant\"
					    }
					  ]
					})";
	print_r($callbackResponse);
} else if ($query != ""){
	$RCommand = "Rscript ReconciliationAPI.R ".escapeshellarg($query);

	$output = shell_exec($RCommand);

	print_r($output);
} else { #just print something and hope it works
	#!!! Make sure $callback is included, otherwise it won't work
	$callbackResponse = "({
					  \"name\" : \"Enipedia Power Plant Reconciliation\",
					  \"identifierSpace\" : \"http://rdf.freebase.com/ns/type.object.mid\",
					  \"schemaSpace\" : \"http://rdf.freebase.com/ns/type.object.id\",
					  \"view\" : {
					    \"url\" : \"http://www.freebase.com/view{{id}}\"
					  },
					  \"preview\" : {
					    \"url\" : \"http://www.freebase.com/widget/topic{{id}}?mode=content\",
					    \"width\" : 430,
					    \"height\" : 300
					  },
					  \"suggest\" : {
					    \"property\" : \"http://localhost/testMatch.php\"
					  },
					  \"defaultTypes\" : [
					    {
					      \"id\" : \"/Category:Powerplant\",
					      \"name\" : \"Powerplant\"
					    }
					  ]
					})";
	print_r($callbackResponse);
}
