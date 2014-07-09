var ROOT_GRAPHS = "./measures/plots/";

var regions = document.querySelectorAll('#Region > div');
var table = document.querySelector("#Main table");
var rows = document.querySelectorAll('#Main table tbody tr');
var Code = document.querySelectorAll(".code");
var Images = document.querySelectorAll("#Region .img-responsive");
var divs = document.querySelectorAll("#Region > div > div");
var navbar = document.querySelector("#navbar");
var navs = document.querySelectorAll("#navbar > li");
var view = "graph1"
var editor = [];

function suppr_whitespace(string) {
    return string.replace(/\s+/,"").replace(/\s+$/,"");
}


for (var i = 1 ; i < rows.length ; i++){
    rows[i].setAttribute('id',regions[i].getAttribute('id'));
}


function set_image(image,nb_invoc) {
    console.log(nb_invoc);
    if (nb_invoc > 1) {
        source = "data:image/png;base64," + image.getAttribute("data");
        image.setAttribute('src', source);
    }
    else {
        parent = image.parentNode;
        parent.removeChild(image);
        parent.innerHTML = "<p style=\"height:300px\">One invocation only</p>";
    }
}


function first_call (j) {
    id = regions[j+1].getAttribute('id')
    Code[j].value = suppr_whitespace(Code[j].value);
    editor[j] = CodeMirror.fromTextArea(Code[j], {
        mode:Code[j].getAttribute("mode"), indentUnit:4,
        autofocus:true, lineNumbers:true, readOnly:true});
    nb_invoc = regions[j+1].getAttribute("data-nb-invoc");
    set_image(Images[2*j], nb_invoc);
    set_image(Images[2*j + 1], nb_invoc);
}


function center_code (j) {
    var Line = parseInt(Code[j].getAttribute('line'));
    editor[j].scrollTo(null,editor[j].heightAtLine(Line-4,mode="local"));
    editor[j].doc.addLineClass(Line-1, "background", "bg-danger")
}


function show_hidden () {
    for( i = 0 ; i < divs.length ; i++) {
        div_id = divs[i].parentNode.getAttribute('id');
        div_data = divs[i].getAttribute("data-name");
        if((((div_data == view)||(div_data == "default"))&&(id_region == div_id))||(div_data == "navbar")) {
            divs[i].className = "";
        }
        else
            divs[i].className = "hidden";
    }
}

function change_view(nav){
    if (nav =="default")
        nav = navs[0].firstChild;
    if (nav =="init")
        nav = navs[2].firstChild;
    view = nav.getAttribute("data-nav");
    for (i=0;i<navs.length;i++) {
        if(navs[i] == nav.parentNode)
            navs[i].className = "active";
        else
            navs[i].className = "";
    }
    show_hidden()
}


rows[1].className = "bg-primary";
id_region = regions[1].getAttribute('id');
first_call(0);
change_view("init");
center_code (0);
change_view("default");


table.onclick = function (event) {
    var row = event.target.parentNode;
    id_region = row.getAttribute('id')
    if (row.getAttribute("data-button") == "True") {
        change_view("init");
        for (var j = 1 ; j < regions.length ; j++){
            rows[j].className = "";
            if(regions[j].getAttribute('id') == id_region) {
                if (!editor[j-1]) {
                    first_call(j-1)
                }
                center_code (j-1);
            }
        }
        if(id_region != "col")
            row.className = "bg-primary";
        change_view("default");
    }
}

navbar.onclick = function (event) {
    change_view(event.target);
}


 
