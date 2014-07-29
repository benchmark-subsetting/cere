var ROOT_GRAPHS = "./measures/plots/";
var COLOR = ["","red","blue","green","yellow","black","grey","pink","maroon","orange","purple","magenta",
             "silver","golden","brown","cyan"]

var regions = document.querySelectorAll('#Region > div');
var table = document.querySelector("#Main table");
var rows = document.querySelectorAll('#Main table tbody tr');
var divs = document.querySelectorAll("#Region > div > div");
var navbar = document.querySelector("#navbar");
var navs = document.querySelectorAll("#navbar > li");
var spans = document.querySelectorAll("#Region table span");
var nas = document.querySelectorAll(".btn-info");
var view = "graph1"
var editor = [];

function init_color() {
    for (i=0;i<spans.length;i++) {
        if(i<16) {
            spans[i].setAttribute("style", "background-color:" + COLOR[spans[i].getAttribute("color")] +
                                  ";" + "color:" + COLOR[spans[i].getAttribute("color")]);
        }
    }
}

function init_nas() {
    for (i=0;i<nas.length;i++) {
        nas[i].parentNode.setAttribute("data-button","False");
        nas[i].innerHTML= "NA"
    }
}

function init() {
    init_color()
    init_nas()
    $(rows[1]).toggleClass("bg-info");
    id_region = regions[1].getAttribute('id');
    first_call(0);
    change_view("init");
    center_code (0);
    change_view("default");
    table.setAttribute("id","treetable")
}

function suppr_whitespace(string) {
    return string.replace(/\s+/,"").replace(/\s+$/,"");
}

function set_image(image,nb_invoc) {
    if (nb_invoc > 1) {
        source = "data:image/png;base64," + image.getAttribute("data");
        image.setAttribute('src', source);
    }
    else {
        parent = image.parentNode;
        parent.removeChild(image);
        if (nb_invoc == 1)
            parent.innerHTML = "<p style=\"height:300px\">One invocation only</p>";
        else
            parent.innerHTML = "<p style=\"height:300px\">No Invocation -> THIS CODELET NOT IN level_*.csv? </p>";
    }
}


function first_call (j) {
    var id = regions[j+1].getAttribute('id');
    var code = $("#"+id+" .code")[0];
    code.value = suppr_whitespace(code.value);
    editor[j] = CodeMirror.fromTextArea(code, {
                mode:code.getAttribute("mode"), indentUnit:4,
                autofocus:true, lineNumbers:true, readOnly:true});
    nb_invoc = regions[j+1].getAttribute("data-nb-invoc");
    images = $("#"+id+" img");
    for (i=0;i<images.length;i++) {
        set_image(images[i], nb_invoc);
    }
}


function center_code (j) {
    var code = $("#"+id_region+" .code")[0];
    var Line = parseInt(code.getAttribute('line'));
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

init();

$("#treetable").treetable({ expandable: true ,indent:8});
var selected = $('tr[data-selected="true"]')
for( i = 0 ; i < selected.length ; i++) {
    node = selected[i].getAttribute("data-tt-parent-id");
    while(node != null) {
        $("#treetable").treetable("expandNode", node);
        parent = $('tr[data-tt-id='+node+']');
        node = parent[0].getAttribute("data-tt-parent-id")
    }
}

table.onclick = function (event) {
    var row = event.target.parentNode;
    if (event.target.getAttribute("strong") == "y")
        row = row.parentNode;
    if (row.getAttribute("data-button") == "True") {
        id_region = row.getAttribute('data-region')
        change_view("init");
        for (var j = 1 ; j < regions.length ; j++){
            if(regions[j].getAttribute('id') == id_region) {
                if (!editor[j-1]) {
                    first_call(j-1)
                }
                center_code (j-1);
            }
        }
        $("tr.bg-info").removeClass("bg-info");
        $(row).toggleClass("bg-info");
        change_view("default");
    }
}

navbar.onclick = function (event) {
    change_view(event.target);
}


 
