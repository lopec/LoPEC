-module (web_info).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"LoPEC".

subtitle() ->
    "".

footer() ->
    "LoPEC 2009".

body() ->
    #rounded_panel { color=gray, body=[
	    #label{text="Dansa!"},
      "This is a rounded panel."
    ]}.
	
event(_) -> ok.
