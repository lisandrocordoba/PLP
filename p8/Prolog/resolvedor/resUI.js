var clauses;
var numclaus1;
var numclaus2;
var lits1;
var lits2;
var resolvente;
var sigma;
var extra = "";

function showClause(line,number,rename){
  var claus = Array.isArray(line) ? line : reunite(line.replace(/ /g, "").split(','));
  if (rename){
    claus = renombrar(claus,number+1);
  }
  clauses.push(claus);
  return ""+(number+1)+': <a onclick="toggleClause('+number+')">'+showC(claus)+"</a><br>";
}

function showFileContents(fileText){
  clauses = new Array();
  borrar(1);
  borrar(2);
  var res = "";
  var lines = fileText.split("\n");
  lines.forEach(function(line,number){if (line.trim().length > 0) res += showClause(line.trim().replace(/¬/g, "-"),number,true);});
  return res+"<br>\n";
}

function esResultado(resultado){
  return (resultado instanceof Object) && (resultado.clause != undefined) && (resultado.subst != undefined);
}

function resolver(idx1, idx2, lit1, lit2){
  var res;
  var claus1 = clauses[idx1];
  var claus2 = clauses[idx2];
  //Si tienen variables libres comunes, se renombra la segunda.
  var inter = intersection(fv(claus1.map(parsear)),fv(claus2.map(parsear)));
  if (inter.length > 0){
    var number = clauses.length + 1;
    claus2 = renombrar(claus2, number, inter);
    lit2 = renombrar(lit2, number, inter);
  }
  
  if (lit1.every(esPositivo) && lit2.every(esNegativo)) {
    res = resolventeYSubst(claus1, claus2, lit1, lit2);
  }
  else if (lit2.every(esPositivo) && lit1.every(esNegativo)) {
    res = resolventeYSubst(claus2, claus1, lit2, lit1);
  }
  else {
    res = "Error: signos incorrectos."
  }
  if (esResultado(res) && (inter.length > 0)){
    res.replaced = inter;
  }
  return res;
}

function finalized(){
  return (clauses != undefined) && (clauses.length > 0) && (clauses[clauses.length-1] != undefined) && ((clauses[clauses.length-1]).length == 0);
}

function updateButtons(){
  document.getElementById("calcular").disabled = (lits1 == undefined)||(lits2 == undefined)||(lits1.length == 0)||(lits2.length == 0) || finalized();
  document.getElementById("agregar").disabled = (resolvente == undefined)||(sigma == undefined) || finalized();
}

function borrarResolvente(){
  resolvente = undefined;
  sigma = undefined;
  document.getElementById("resolvente").innerHTML = "&nbsp;";
  updateButtons();
}

function borrar(unoODos){//Indirectamente actualiza los botones al borrar el resolvente.
  if (unoODos == 1){
    numclaus1 = undefined;
    lits1 = undefined;
  }
  else {
    numclaus2 = undefined;
    lits2 = undefined;
  }
  borrarResolvente();
  document.getElementById("claus"+unoODos).innerHTML = "&nbsp;";
}

function poner(unoODos,num){
  if (num < clauses.length){//Solo por seguridad, para prevenir errores.
    if (unoODos == 1){
      numclaus1 = num;
      lits1 = new Array();
    }
    else {
      numclaus2 = num;
      lits2 = new Array();
    }
    borrarResolvente();
    var html = showOnRule(clauses[num],num,unoODos);
    document.getElementById("claus"+unoODos).innerHTML = html;
  }
}

function showLiteralOnRule(e,index,num,unoODos){
  return '<a class="plain" id="idx-'+num+'-'+index+'" onclick="toggleIndex('+num+','+index+','+unoODos+');">'+subindex(e.replace("-","&not;"))+"</a>";
}

function showOnRule(claus,num,unoODos){
  if (!Array.isArray(claus))
    return '<span style="color:red;">'+claus+'</span>';
  if (claus.length == 0)
    return "&square";
  var res = "{";
  res += claus.map(function(e,index){return showLiteralOnRule(e,index,num,unoODos);}).join(", ");
  res += "}";
  return res;
}

function toggleClause(num){
  if (finalized()){
    return;
  }
  if (numclaus1 == num){//Si es undefined da false.
    borrar(1);
  }
  else if (numclaus2 == num){
    borrar(2);
  }
  else if (numclaus1 == undefined){
    poner(1,num);
  }
  else{
    poner(2,num);
  }
}

function resolventeValido(){
  return (resolvente != undefined && ((clauses.filter(function(e){return equals(e,resolvente);})).length == 0));
}

function agregarResolvente(){
  if (resolventeValido()){
    var list = document.getElementById("list");
    list.innerHTML += "De "+(numclaus1+1)+" y "+(numclaus2+1)+" con "+showS(sigma) + extra +":<br>";
    list.innerHTML += "&nbsp;&nbsp;"+showClause(resolvente,clauses.length,false)+"<br>\n";
    borrar(1);
    borrar(2);
  }
}

function toggleIndex(num,index,unoODos){
  var lits = (unoODos == 1) ? lits1 : lits2;
  var lit = (clauses[num])[index];
  if (lits.indexOf(lit) >= 0){//Si estaba...
    removeFirst(lit,lits);
  }
  else{
    lits.push(lit);
  }
  $("#idx-"+num+'-'+index).toggleClass("plain");
  $("#idx-"+num+'-'+index).toggleClass("selected");
  borrarResolvente();
}

function calcularResolvente(){
  var resultado = resolver(numclaus1, numclaus2, lits1, lits2);
  if (esResultado(resultado)){
    extra = resultado.replaced == undefined ? "" : '<span class="renombre"> (renombrando '+subindex(resultado.replaced.toString().replace(/,/g,", "))+" en "+(numclaus2+1)+")</span>";
    resolvente = resultado.clause;
    sigma = resultado.subst;
    document.getElementById("resolvente").innerHTML = showC(resolvente);
    $("#resolvente").removeClass("error");
  }
  else {
    resolvente = undefined;
    sigma = undefined;
    document.getElementById("resolvente").innerHTML = resultado;
    $("#resolvente").addClass("error");
  }
  updateButtons();
}