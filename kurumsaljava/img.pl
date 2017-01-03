chdir("pub");
for $file(<page*.jpg>) {
    $newfile = "slide" . $file;
    system("convert -resize %40 $file $newfile");
}
