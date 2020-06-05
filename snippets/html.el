(defun html-5 ()
  "Insert a template for an empty HTML page"
  (interactive)
  (insert "<!DOCTYPE html>\n"
	  "<html lang='en'>\n"
	  "<head>\n"
	  "<meta charset='UTF-8'>\n"
	  "<meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
	  "<meta http-equiv='X-UA-Compatible' content ='ie=edge'>\n"
	  "<title></title>\n"
	  "</head>\n\n"
	  "<body>\n\n"
	  "</body>\n"
	  "</html>\n")
  ) 
