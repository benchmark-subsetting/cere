var ROOT_GRAPHS = "./measures/plots/";
var COLOR = ["","red","blue","green","yellow","black","grey","pink","maroon","orange","purple","magenta",
             "silver","golden","brown","cyan"]
var view = "graph1";
var id_region;
var editor = [];

//initialise the color for cluster in invocation table
function init_color() {
    $("#Region table span").each(function() {
        if(this.getAttribute("color")<16) {
            this.setAttribute("style", "background-color:" + COLOR[this.getAttribute("color")] +
                                  ";" + "color:" + COLOR[this.getAttribute("color")]);
        }
    });
}

//initialise the view of report
function init() {
    init_color()
    //select the first row of table
    row = $('#Main table tbody tr')[1]
    $(row).toggleClass("bg-info");
    id_region = row.getAttribute('data-region');
    //initialise the correspondant region
    first_call();
    //initialise the view
    change_view($('.nav li.active > a'));
    change_view($('.nav li.active > a'))
    change_view($('.nav li.active > a'))
}

function set_image(image,nb_invoc) {
    // if nb_invocation different of 1 initialise image
    var tab = (image.parentNode.getAttribute('data-name'));
    if (tab == "Call_graph") return;
    if (nb_invoc > 1) {
        source = "data:image/png;base64," + image.getAttribute("data");
        image.setAttribute('src', source);
    }
    //else remove image and print error message
    else {
        parent = image.parentNode;
        parent.removeChild(image);
        if (nb_invoc == 1)
            parent.innerHTML = "<p style=\"height:300px\">One invocation only</p>";
        else
            parent.innerHTML = "<p style=\"height:300px\">No Invocation -> THIS CODELET NOT IN level_*.csv? </p>";
    }
}


//initialise region j
function first_call () {
    //initialise codemirror editor
    var code = $("#"+id_region+" .code")[0];
    editor[id_region] = CodeMirror.fromTextArea(code, {
                        mode:code.getAttribute("mode"), indentUnit:4,
                        autofocus:true, lineNumbers:true, readOnly:true});
    
    //initialise image
    nb_invoc = $("#Region > div[id="+id_region+"]")[0].getAttribute("data-nb-invoc");
    images = $("#"+id_region+" img");
    for (i=0;i<images.length;i++) {
        set_image(images[i], nb_invoc);
    }
}


function center_code () {
    //focus codemirror editor to wanted line and highlight 
    var code = $("#"+id_region+" .code")[0];
    var Line = parseInt(code.getAttribute('line'));
    editor[id_region].scrollTo(null,editor[id_region].heightAtLine(Line-4,mode="local"));
    editor[id_region].doc.addLineClass(Line-1, "background", "bg-danger")
}


//show or hidden the different block in html page
function show_hidden () {
    $("#Region > div > div").each(function() {
        div_id = this.parentNode.getAttribute('id');
        div_data = this.getAttribute("data-name");
        if((((div_data == view)||(div_data == "default"))&&(id_region == div_id))||(div_data == "navbar")) {
            this.className = "";
        }
        else
            this.className = "hidden";
    });
}

//change view to the wanted tab
function change_view(nav){
    var navs = $("#navbar > li");
    view = nav.attr('data-nav')
    if (view == "Code") {
        center_code();
    }
    show_hidden();
}

init();

//initialise treetable
table = $("#Main table");
table.treetable({ expandable: true ,indent:8});
var selected = $('tr[data-selected="true"]')
for( i = 0 ; i < selected.length ; i++) {
    node = selected[i].getAttribute("data-tt-parent-id");
    while(node != null) {
        $("#Main table").treetable("expandNode", node);
        parent = $('tr[data-tt-id='+node+']');
        node = parent[0].getAttribute("data-tt-parent-id")
    }
}


//manage click on treetable
table[0].onclick = function (event) {
    var row = event.target.parentNode;
    if (event.target.getAttribute("strong") == "y")
        row = row.parentNode;
    if (row.getAttribute("data-button") == "True") {
        id_region = row.getAttribute('data-region');
        if (!editor[id_region]) {
            first_call()
            }
        $("tr.bg-info").removeClass("bg-info");
        $(row).toggleClass("bg-info");
        change_view($('.nav li.active > a'));
        change_view($('.nav li.active > a'))
        change_view($('.nav li.active > a'))
    }
}

$('.nav li').click(function(){
    $(this).addClass('active').siblings().removeClass('active');
    change_view($('.nav li.active > a'))
    change_view($('.nav li.active > a'))
    change_view($('.nav li.active > a'))
})
