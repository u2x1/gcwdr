function decrypt() {
  var passwd = document.getElementById("passwd").value;
  var origin = document.getElementById("article-content").innerText;
  var newStr = "";
  for (i = 0; i < origin.length; i++) {
    newStr = newStr + String.fromCharCode(origin.charCodeAt(i) - passwd.charCodeAt(i % passwd.length));
  }
  document.getElementById("article-content").innerHTML = newStr;
}

function reset() {
  var passwd = document.getElementById("passwd").value;
  var origin = document.getElementById("article-content").innerHTML;
  var newStr = "";
  for (i = 0; i < origin.length; i++) {
    newStr = newStr + String.fromCharCode(origin.charCodeAt(i) + passwd.charCodeAt(i % passwd.length));
  }
  document.getElementById("article-content").innerHTML = newStr;
}

function hidePasswdDiv() {
  var par = document.getElementById("article");
  par.removeChild(document.getElementById("passwd-div"));
}
