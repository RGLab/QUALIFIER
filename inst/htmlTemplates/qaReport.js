
var bg="#7896b1";
var fg="#c9d8e5";


function toggleTable(id){
    var hide = "none";
    var show = "inline";
    var obj = document.getElementById("table_"+id);
    var state = obj.style.display;
    if(state == show){
	obj.style.display=hide;
    }else{
	obj.style.display=show;
    }

//var inTrig;
//var outTrig;
//
//inTrig = document.getElementById("detTriggerIn_"+id);
//outTrig = document.getElementById("detTriggerOut_"+id);
//if(state == show){
//inTrig.style.display=show;
//outTrig.style.display=hide;
//}else{
//inTrig.style.display=hide;
//outTrig.style.display=show;
//}

}

function toggleSection(id){
    var hide = "none";
    var show = "inline";
    var obj = document.getElementById("section_"+id);
    var state = obj.style.display;
    if(state == show){
	obj.style.display=hide;
    }else{
	obj.style.display=show;
    }

var inTrig;
var outTrig;

inTrig = document.getElementById("detTriggerIn_"+id);
outTrig = document.getElementById("detTriggerOut_"+id);
if(state == show){
inTrig.style.display=show;
outTrig.style.display=hide;
}else{
inTrig.style.display=hide;
outTrig.style.display=show;
}

}


function toggleDetails(iRange, jRange, k, id, nrPages){
    var hide = "none";
    var show = "block";
    var state =  document.getElementById("button_"+k+"_1_1").style.display;
    for(var i=1; i<=iRange; i++){
	for(var j=1; j<=jRange; j++){
	    var obj = document.getElementById("button_"+k+"_"+i+"_"+j);
	    var image = document.getElementById("img_"+k+"_"+i+"_"+j);
	    if(state == show){
		obj.style.display=hide;
		image.style.display=hide;
	    }else{
		obj.style.display=show;
	    }
	}
    }
    var inTrig;
    var outTrig;
    for(var l=1; l<=nrPages; l++){
	inTrig = document.getElementById(id+"_detTriggerIn_"+l);
	outTrig = document.getElementById(id+"_detTriggerOut_"+l);
	if(state == show){
	    inTrig.style.display=show;
	    outTrig.style.display=hide;
	}else{
	    inTrig.style.display=hide;
	    outTrig.style.display=show;
	}
    }
}



function togglePages(from, to, nrPages, nrFrames){
    var hide = "none";
    var show = "table-row";
    for(var i=1; i<=nrFrames; i++){
	var row1 = document.getElementById("frow1_"+i);
	var row2 = document.getElementById("frow2_"+i);
	row1.style.display=hide;
	row2.style.display=hide;
    }
	
    for(var j=from; j<=to; j++){
	var row1 = document.getElementById("frow1_"+j);
	var row2 = document.getElementById("frow2_"+j);
	row1.style.display=show;
	row2.style.display=show;
    }
	  
}


function link2Panel(panel, frame) {
  window.location.href = "index" + panel + ".html#frow1_" + frame;
}


