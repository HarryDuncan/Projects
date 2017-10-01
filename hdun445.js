// JavaScript source code section for UI/UX
function showComments() {
  document.getElementById("booksSection").style.display='none';
  document.getElementById("userReg").style.display='none';
  document.getElementById("blueraySection").style.display='none';
  document.getElementById("comments").style.display='block';
  }

  function showBooks() {
  getBooks();
  document.getElementById("comments").style.display='none';
  document.getElementById("userReg").style.display='none';
  document.getElementById("blueraySection").style.display='none';
  document.getElementById("booksSection").style.display='block';
  document.getElementById('search').value = "";
  books = true;
  br = false;

  }
  function showBlueray() {
  getBR();
  document.getElementById("comments").style.display='none';
  document.getElementById("userReg").style.display='none';
  document.getElementById("booksSection").style.display='none';
  document.getElementById("blueraySection").style.display='block';
  document.getElementById('search').value = "";
  br = true;
  books = false;
  }

  function showUserReg() {
  document.getElementById("booksSection").style.display='none';
  document.getElementById("comments").style.display='none';
  document.getElementById("blueraySection").style.display='none';
  document.getElementById("userReg").style.display='block';
  }

  //Global Variables
  var books = true;
  var br = false;
  var resp;
  var bookList;

  //Script source code for Back-end dev
  
 
  //Gets the books from the server
  function getBooks(){
		var xhr = new XMLHttpRequest();
		var uri =  "http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/booklist";
		xhr.open("Get",uri, true);
		xhr.setRequestHeader('Accept', 'application/json');
		xhr.onload = function(){
			// Gets the http response and turns it into a JSON object
			var resp = xhr.responseText;
			var booksData = JSON.parse(resp);
			//Gets the data into a table
			var tablecontent = "<tr id= 'noShow'><td >" +  + "</td><td >" +  + "</td><td >" +  + "</td><td >" +  + "</td></tr>\n";
				for(var i=0; i<booksData.length;i++){
				var record= booksData[i];
				tablecontent += "<tr class='product'><td class='authorName'>" + record.AuthorInitials + " " + record.AuthorSurname + "</td><td class = 'noShow'>"+ record.AuthorInitials + "</td><td class = 'noShow'>" + record.AuthorSurname + "</td><td class = 'noShow'>" + record.Id + "</td><td class = 'bookTitle'>"+record.Title +"</td><td class='displayPic'><img src= 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/bookimg?id="+record.Id+"'></td><td><button onclick='buyBook(\""+String(record.Id)+"\");'>Buy Now</button></td></tr>\n";
				}
				document.getElementById("booksTab").innerHTML = tablecontent;
		}
		xhr.send(null);
  }
  

   //Gets the blueRay from the server
  function getBR(){
		var xhr = new XMLHttpRequest();
		var uri =  "http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/brlist";
		xhr.open("Get",uri, true);
		xhr.setRequestHeader('Accept', 'application/json');
		xhr.onload = function(){
			// Gets the http response and turns it into a JSON object
			var resp = xhr.responseText;
			var bRData = JSON.parse(resp);
			//Gets the data into a table
			var tablecontent ="<tr id= 'noShow'><td >" +  + "</td><td >" +  + "</td></tr>\n";;
				for(var i=0; i<bRData.length;i++){
				var record= bRData[i];
				tablecontent += "<tr class='product'><td class='brTitle'>" + record.Title + "</td><td class = 'noShow'>" + record.Id + "</td><td class='displayPic'><img src= 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/brimg?id="+record.Id+"'></td><td><button onclick='buyBluray(\""+record.Id+"\");'>Buy Now</button></td></tr>\n";
				//gets the image for the item
				}
				document.getElementById("brTab").innerHTML = tablecontent;
		}
		xhr.send(null);
  }


  //Funtion for posting comments to the iframe and reloading 
  function postComment(comment, name){
		var xhr = new XMLHttpRequest();
		console.log(name);
		var uri = 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/comment?name='+name;
		xhr.open("POST", uri, true);
		xhr.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
		xhr.onload = function(){
		}
		xhr.send(JSON.stringify(comment));
		document.getElementById('commentDisp').src = document.getElementById('commentDisp').src;
  }

  //Function for the search bar
  function searchItem(value){
	//Makes sure spaces are replaced with +
	var term ="";
	for (var i = 0, len = value.length; i < len; i++) {
		if(value[i] == " "){
			console.log("space");
			term =+ "+";
		}
		else{
			term += value[i];
		}
	}
		//Determines if the request if for books or blueRay because the nav bar is global and fixed
	if(books == true){
			var xhr = new XMLHttpRequest();
			var uri = 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/booksearch?term='+value;
			xhr.open("GET", uri, true);
			xhr.setRequestHeader("Accept", "application/json");
			xhr.onload = function(){
					// Gets the http response and turns it into a JSON object
					var resp = xhr.responseText;
					var booksData = JSON.parse(resp);
					//Gets the data into a table
					var tablecontent = "<tr id= 'noShow'><td >" +  + "</td><td >" +  + "</td><td >" +  + "</td><td >" +  + "</td></tr>\n";
					for(var i=0; i<booksData.length;i++){
						var record= booksData[i];
						tablecontent += "<tr class='product'><td class='authorName'>" + record.AuthorInitials + " " + record.AuthorSurname + "</td><td class = 'noShow'>" + record.AuthorInitials + "</td><td class = 'noShow'>" + record.AuthorSurname + "</td><td class = 'noShow'>" + record.Id + "</td><td class = 'bookTitle'>"+ record.Title +"</td><td class='displayPic'><img src= 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/bookimg?id="+record.Id+"'></td><td><button onclick='buyBook(\""+String(record.Id)+"\");'>Buy Now</button></tr>\n";
					}
					document.getElementById("booksTab").innerHTML = tablecontent;
			}
			xhr.send(null);
	}else{
			var xhr = new XMLHttpRequest();
			var uri = 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/brsearch?term='+value;
			xhr.open("GET", uri, true);
			xhr.setRequestHeader("Accept", "application/json");
			xhr.onload = function(){
					// Gets the http response and turns it into a JSON object
					var resp = xhr.responseText;
					var bRData = JSON.parse(resp);
					//Gets the data into a table
					var tablecontent ="<tr id= 'noShow'><td >" +  + "</td><td >" +  + "</td></tr>\n";;
				for(var i=0; i<bRData.length;i++){
				var record= bRData[i];
				tablecontent += "<tr class='product' ><td class='brTitle'>" + record.Title + "</td><td class = 'noShow'>" + record.Id + "</td><td class='displayPic' ><img src='http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/brimg?id="+record.Id+"'></td><td><button onclick='buyBluray(\""+String(record.Id)+"\");'>Buy Now</button> </td></tr>\n";
				}
				document.getElementById("brTab").innerHTML = tablecontent;
		}
		xhr.send(null);
	
		}
	}


	//Function to register a new user
	function registerUser(name,pass, address){
		var xhr = new XMLHttpRequest();
		var uri = 'http://redsox.uoa.auckland.ac.nz/BC/Open/Service.svc/register';
		xhr.open("POST", uri, true);
		xhr.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
		xhr.onload = function(){
		var resp = xhr.response;
		alert(resp);
		}
		var ob = {
					"Address":address,
					"Name":name,
					"Password":pass
				}
				
		var userOb = JSON.stringify(ob);
		xhr.send(userOb);
		
		
		
  }

	//Function to log in a user
	function userAuthenticate(){
		window.open('http://redsox.uoa.auckland.ac.nz/BC/Closed/Service.svc/user', "_blank")
	}

	function buyBook(ID){
		window.open('http://redsox.uoa.auckland.ac.nz/BC/Closed/Service.svc/bookbuy?id='+ID,"_blank");
	}

	function buyBluray(ID){
		window.open('http://redsox.uoa.auckland.ac.nz/BC/Closed/Service.svc/brbuy?id='+ID,"_blank");
	}
  document.onload = getBooks();