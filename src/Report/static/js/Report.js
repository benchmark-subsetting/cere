var ROOT_GRAPHS = "./measures/plots/";

var regions = document.querySelectorAll('#Region > div');
var table = document.querySelector("#Main table");
var rows = document.querySelectorAll('#Main table tbody tr');
var Code = document.querySelectorAll(".code");
var Images = document.querySelectorAll(".img-responsive");
var editor = [];

function suppr_whitespace(string) {
    return string.replace(/\s+/,"").replace(/\s+$/,"");
}


for (var i = 1 ; i < rows.length ; i++){
    rows[i].setAttribute('id',regions[i-1].getAttribute('id'));
}



function first_call (j) {
    Code[j].value = suppr_whitespace(Code[j].value);
    editor[j] = CodeMirror.fromTextArea(Code[j], {
        mode:Code[j].getAttribute("mode"), indentUnit:4,
        autofocus:true, lineNumbers:true, /*readOnly:true*/ });
    Images[2*j].setAttribute('src', ROOT_GRAPHS+regions[j].getAttribute('id')+"_byPhase.png");
    Images[2*j + 1].setAttribute('src', ROOT_GRAPHS+regions[j].getAttribute('id')+".png");
}


function center_code (j) {
    var Line = parseInt(Code[j].getAttribute('line'));
    editor[j].scrollTo(null,editor[j].heightAtLine(Line-4,mode="local"));
    editor[j].doc.addLineClass(Line-1, "background", "bg-danger")
}


rows[1].className = "bg-primary";
regions[0].className = "None";
first_call(0);
center_code (0);
for (var j = 1 ; j < regions.length ; j++){
    regions[j].className = "hidden";
}


table.onclick = function (event) {
    var row = event.target.parentNode;
    if (event.target.getAttribute("data-button") == "True") {
        for (var j = 0 ; j < regions.length ; j++){
            rows[j+1].className = "";
            if(regions[j].getAttribute('id') == row.getAttribute('id')) {
                regions[j].className = "None";
                if (!editor[j]) {
                    first_call(j)
                }
                center_code (j);
            }
            else
                regions[j].className = "hidden";
        }
        if(row.getAttribute('id') != "col")
            row.className = "bg-primary";
    }
    if(editor[0] == editor[1])
        print ("naze");
}
 
