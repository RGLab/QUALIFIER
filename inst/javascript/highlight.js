
    var SVGDocument = null;
    var SVGRoot = null;
    var SVGViewBox = null;
    var svgns = 'http://www.w3.org/2000/svg';
    var xlinkns = 'http://www.w3.org/1999/xlink';
    var toolTip = null;
    var TrueCoords = null;
    var tipBox = null;
    var tipText = null;
    var tipTitle = null;
    var tipDesc = null;

    var lastElement = null;
    var titleText = '';
    var titleDesc = '';

	function showPlot(evt, img_link) {
	
	    // get a pointer to the parent HTML document
	    var doc = top.document;

	    // check if we already have a '_popup' element in the DOM;
	    // if we do, remove it
	    var old_img = doc.getElementById('_popup');
	    if (old_img) {
		doc.body.removeChild(old_img);

		// if the source of 'old_img' is the same as 'img_link',
		// then we've clicked on the same point twice, and we
		// probably want to hide that image without re-generating
		// a new one
		if (old_img.getAttribute("src") == img_link) {
		    return;
		}
	    }

	    var img = doc.createElement("img");

	    // set the src first so we can query the width, height
	    img.setAttribute("src", img_link);

	    // get the width, height of the image
	    var img_width = img.naturalWidth;
	    var img_height = img.naturalHeight;

	    // get the window dimensions
	    var window_width = top.window.innerWidth;
	    var window_height = top.window.innerHeight;

	    // this may need to be tweaked to get it to display
	    // in the correct position
	    img.setAttribute("style",
		"position: fixed;" +
		"left: " + (window_width/2 - img_width/2) + "px;" +
		"top: " + (window_height/2 - img_height/2) + "px;"
	    )

	    // set the id, src so it displays the right image
	    img.setAttribute("id", "_popup");
	    img.setAttribute("src", img_link);
	    img.setAttribute(
		"onclick",
		"document.body.removeChild( document.getElementById('_popup') );"
	    );

	    // append it to the bottom of the body
	    doc.body.appendChild(img);
	};
	function hightlightBox(el, status)
	{

	    if(status) {
	    
	      el.setAttribute('fill', 'red');

	      el.setAttribute('fill-opacity', 1);
	      el.setAttribute('height', 1.5*el.getAttribute('oldheight'));


	    }
	    else{ 
		el.setAttribute('fill', el.getAttribute('oldfill'));

		if(el.nodeName=="rect")	      
			el.setAttribute('fill-opacity', 1);
		else
			el.setAttribute('fill-opacity', 0);

		}
		el.setAttribute('height', el.getAttribute('oldheight'));

	};
	
	function setBoxStyle(evt,status)
	{
          var targetElement = evt.target;
	  var titleValue = '';
	  var targetTitle = targetElement.getElementsByTagNameNS(svgns, 'title').item(0);
	  if ( targetTitle )
	  {
	
	     titleValue = targetTitle.firstChild.nodeValue;
	  }
	
	
	  if ( '' != titleValue&&titleValue!='R SVG Plot' )
	  {
	    var sampleID=titleValue.split(" ")[1].split("=")[1];

	 	var i;
		SVGDocument = evt.target.ownerDocument;
		SVGRoot = SVGDocument.documentElement; 
		var nodes=SVGRoot.getElementsByTagNameNS(svgns, 'a');

	       for(i = 0; i <nodes.length ; i++) {
			curNode=nodes.item(i);
			 var id =  curNode.getAttribute("xlink:href").split("_")[1];
			 if(id==sampleID)
			  {
				targetMean=curNode.childNodes[1];
				targetbox=curNode.previousSibling;
				//search for the nearest polyline before current a				
				while(targetbox.nodeName!="polyline")
				{
				 targetbox=targetbox.previousSibling;						
				}

				hightlightBox(targetMean, status);
				hightlightBox(targetbox, status);
			}
		}
	  }
	};
	function reset_Box(evt)
	{
          setBoxStyle(evt,0);

	};	
   
     function  highlight_Box(evt)
     {
	setBoxStyle(evt,1);
	};

    function backupBoxStyle(evt)
   {
	SVGDocument = evt.target.ownerDocument;
	SVGRoot = SVGDocument.documentElement; 
	var nodes=SVGRoot.getElementsByTagNameNS(svgns, 'polyline');


	for(i = 0; i <nodes.length ; i++) {
		curNode=nodes.item(i);
		var cur = curNode.getAttribute('fill');
	        curNode.setAttribute('oldfill', cur);
		curNode.setAttribute('oldheight', curNode.getAttribute('height'));
		curNode.setAttribute('oldstroke', curNode.getAttribute('stroke'));
	       }
	

	var nodes=SVGRoot.getElementsByTagNameNS(svgns, 'rect');
	for(i = 0; i <nodes.length ; i++) {
		curNode=nodes.item(i);
		var cur = curNode.getAttribute('fill');
	        curNode.setAttribute('oldfill', cur);
		curNode.setAttribute('oldheight', curNode.getAttribute('height'));

	       }
	};
	function highlightPoint(el, status)
	{
	  // var old = el.getAttribute('oldfill');

	    //if(status && old == null) 
	      //el.setAttribute('oldfill', el.getAttribute('fill'));

	    if(status) {
	    
	      //el.setAttribute('fill', 'purple');
	      //el.setAttribute('r', 1.5*el.getAttribute('oldr'));
	      el.setAttribute('fill-opacity', 0);

	    }
	    else{ 
		//el.setAttribute('fill', el.getAttribute('oldfill'));
		//el.setAttribute('r', el.getAttribute('oldr'));
	      el.setAttribute('fill-opacity', 1);
		}
	};
	
	function setCircelStyle(evt,status)
	{
          var targetElement = evt.target;
	  var titleValue = '';
	  var targetTitle = targetElement.getElementsByTagNameNS(svgns, 'title').item(0);
	  if ( targetTitle )
	  {
	
	     titleValue = targetTitle.firstChild.nodeValue;
	  }
	
	
	  if ( '' != titleValue )
	  {
	    var uniqueID=titleValue.split(" ")[0].split("=")[1];

	 	var i;
		SVGDocument = evt.target.ownerDocument;
		SVGRoot = SVGDocument.documentElement; 
		var nodes=SVGRoot.getElementsByTagNameNS(svgns, 'circle');

	       for(i = 0; i <nodes.length ; i++) {
			curNode=nodes.item(i);
			curTitle=curNode.getElementsByTagNameNS(svgns, 'title').item(0);
			curTitleValue=curTitle.firstChild.nodeValue;
			 var id = curTitleValue.split(" ")[0].split("=")[1];
			 if(id!=uniqueID)
			  highlightPoint(curNode, status);
		}
	  }
	};
	function reset_circle(evt)
	{
          setCircelStyle(evt,0);

	};	
   
     function  highlight_circle(evt)
     {
	setCircelStyle(evt,1);
	};

    function backupCircleStyle(evt)
   {
	SVGDocument = evt.target.ownerDocument;
	SVGRoot = SVGDocument.documentElement; 
	var nodes=SVGRoot.getElementsByTagNameNS(svgns, 'circle');


	for(i = 0; i <nodes.length ; i++) {
		curNode=nodes.item(i);
		var cur = curNode.getAttribute('fill');
	        curNode.setAttribute('oldfill', cur);
		curNode.setAttribute('oldr', curNode.getAttribute('r'));
	       }
	};

    function Init(evt)
    {
       SVGDocument = evt.target.ownerDocument;
       SVGRoot = SVGDocument.documentElement;
       TrueCoords = SVGRoot.createSVGPoint();

       //window.status = (TrueCoords);

       //create event for object
       SVGRoot.addEventListener('mousemove', highlight_Box, false);
       SVGRoot.addEventListener('mouseout', reset_Box, false);
       SVGRoot.addEventListener('mousemove', highlight_circle, false);
       SVGRoot.addEventListener('mouseout', reset_circle, false);

       backupCircleStyle(evt);
       backupBoxStyle(evt);
    };

