<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Pretty YAML</title>
</head>
<body>
<form method="post" action="." enctype="multipart/form-data">
File name: <input type="file" name="yaml">
<input type="submit">
</form>
$if(yaml)$
<div>$yaml$</div>
$endif$
</body>
</html>
