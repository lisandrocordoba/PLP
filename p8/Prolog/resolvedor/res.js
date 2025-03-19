function esVariable(str){
  if ("string" != typeof str)
    return false;
  let head = str.charAt(0);
  return head >= "A" && head <= "Z";
}

function esExpr(parsed){
  //return arr.length == 1 && Array.isArray(arr[0]) && arr[0].length == 2;
  return parsed instanceof Object && (parsed.symbol != undefined) && (parsed.args != undefined);
}

function esNegativo(pred){
  return pred.charAt(0) == '-';
}

function esPositivo(pred){
  return !esNegativo(pred);
}

function equals(arr1, arr2){
  return JSON.stringify(arr1) === JSON.stringify(arr2);
}

function applyS(expr, subst){//La hice primero sin parsear... Feo, pero anda.
  let res;
  if (expr.includes('[')){//Para listas
    expr = translateList(expr);
  }
  if (Array.isArray(expr)){
    res = applyS(show(expr), subst);
  }
  else if (esVariable(expr)){
    let replacement = subst.get(expr);
    if (replacement != undefined) {
      res = replacement;
    }
    else res = expr;
  }
  else if (expr.includes('(')&&expr.endsWith(')')){//No se acepta basura al final.
    let open = expr.indexOf('(');
    let prefix = expr.substring(0,open+1);
    let content = expr.substring(open+1,expr.length-1);
    res = prefix+applyAll(content,subst)+')';
  }
  else res = expr;
  return show(parsear(res));
}

function applyAll(str, subst){
  let all = reunite(str.split(','));
  function applySubst(target){
    return applyS(target,subst);
  }
  let allApplied = all.map(applySubst);
  return allApplied.toString();
}

function contar(str, chr){
  let res = 0;
  for (j = 0; j < str.length; ++j){
    if (str.charAt(j)==chr){
      ++res;
    }
  }
  return res;
}

function reunite(arr){
  let res = new Array();
  for (i = 0; i < arr.length; ++i){
    let item = arr[i];
    if (item.includes('(')){
      let newItem = item;
      let count = contar(item,'(');
      while (i < arr.length && count > 0){
	count -= contar(item,')');
	if (count > 0){
	  item = arr[++i];
	  count += contar(item,'(');
	  newItem += ","+item;
	}
      }
      res.push(newItem);
    }
    else res.push(item);
  }
  return res;
}

function parsear(expr){
  let res = new Array();
  if (expr.includes('[')){//Para listas
    expr = translateList(expr);
  }
  if (expr.includes('(')&&expr.endsWith(')')&&!esVariable(expr)){//No se acepta basura al final ni predicados variables.
     let open = expr.indexOf('(');
     let prefix = expr.substring(0,open);
     let content = expr.substring(open+1,expr.length-1);
     let all = reunite(content.split(','));
     res = {symbol: prefix, args: (all.map(parsear))};
  }
  else
    res = expr;
  return res;
}

function translateList(expr) {
  let res = "";
  for (let j = 0 ; j < expr.length; ++j){
    let ch = expr[j];
    if (ch == "["){
      let i = j+1;
      if (i < expr.length && expr[i] == "]"){//La lista vacía se mantiene.
	res += "[]";
	++j;
      }
      else {
	res += ".(";
      }
    }
    else if (ch == "]"){
	res += ")";
    }
    else if (ch == "|"){
	res += ",";
    }
    else {
	res += ch;
    }
  }
  return res;
}

function show(parsed){
  let res = "";
  if (esExpr(parsed)){
    if ((parsed.symbol == ".")&& (parsed.args.length == 2)){
      res = "["+show(parsed.args[0])+"|"+show(parsed.args[1])+"]";
    }
    else{
      res = parsed.symbol+"("+show(parsed.args)+")";
    }
  }
  else if (Array.isArray(parsed)){
    res = parsed.map(show).join(",");
  }
  else
    res = parsed;
  return res;
}

function subindex(str){
  let matches = str.match(/[0-9]+/g);
  let res = str;
  if (Array.isArray(matches)){
    nub(matches).forEach(function(x,n){res = replaceAll(x,"<sub>"+x+"</sub>",res)});
  }
  return res;
}

function showS(subst){
  const symbol = ":=";//Antes era "&leftarrow;".
  if (!(subst instanceof Map))
    return '<span style="color:red;">'+subst+'</span>';
  let res = "{";
  let arr = new Array();
  subst.forEach(function(value, key, subst){arr.push(key + " "+symbol+" " + value);});
  res += arr.join(", ");
  res += "}";
  return subindex(res);
}

function showC(claus){
  if (!Array.isArray(claus))
    return '<span style="color:red;">'+claus+'</span>';
  if (claus.length == 0)
    return "&square;";
  let res = "{";
  res += claus.map(function(e){return e.replace("-","&not;")}).join(", ");
  res += "}";
  return subindex(res);
}

function showPair(pair){
  return pair[0]+" &esdot; "+pair[1];
}

//Aplica subst1 a cada valor de subst2, y luego une el resultado con subst1. 
//Asume que los dominios son disjuntos.
function compose(subst1, subst2){
  res = new Map(subst2);
  function applyOne(value, key, subst){
    res.set(key,applyS(value,subst1));
  }
  subst2.forEach(applyOne);
  return new Map([...res, ...subst1]);
}

function removeFirst(item, arr){//No hace nada si el elemento no pertenece al arreglo.
  arr.splice(arr.indexOf(item), 1);
}

function removeIntersection(arr1, arr2){
  arr2.forEach(function(elem){ removeFirst(elem,arr1); });
}

function nub(arr){
  return arr.filter(function(item, pos) { return arr.indexOf(item) == pos; });
}

function copy(arr){
  return arr.map(function(e){return e;});
}

function concatAll(arr){
  return arr.reduce(function(e1,e2) {return e1.concat(e2);});
}

function concatMap(f,arr){
  return concatAll(arr.map(f));
}

function nthPosition(substr,str,n){//Con n contando desde 0.
  let i = 0;
  let pos = 0;
  while (i <= n && pos >= 0){
    pos = (pos == 0) ? pos : pos + substr.length;
    pos = str.indexOf(substr,pos);
    ++i;
  }
  return pos;
}

function replaceNth(substr,replacement,str,n){
  let res = str;
  let pos = nthPosition(substr,str,n);
  if(pos >= 0){
    let prefix = str.slice(0,pos);
    let suffix = str.slice(pos+substr.length,str.length);
    res = prefix+replacement+suffix;
  }
  return res;
}

function replaceAll(substr,replacement,str){
  return str.replace(new RegExp(substr, 'g'), replacement);
}

function anyInCommon(arr1, arr2){
  let res = false;
  for (j = 0; j < arr1.length && !res; ++j){
    res = (arr2.indexOf(arr1[j]) >= 0);
  }
  return res;
}

function intersection(arr1, arr2){//Asume que no hay repetidos.
  return arr1.filter(function(e){ return arr2.indexOf(e) >=0; });
}

//Para MGU
function swap(pair){
  return new Array(pair[1],pair[0]);
}

function decompose(contents1, contents2, pairs) {//Asume que tienen la misma longitud.
  for (i = 0; i < contents1.length; ++i){
    let item = new Array(show(contents1[i]),show(contents2[i]));
    pairs.push(item);
  }
  return pairs;
}

function pertenece(variable, parsed){
  let res = false;
  if (esVariable(parsed)){
    return variable == parsed;
  }
  else if (esExpr(parsed)){
    return pertenece(variable, parsed.args);
  }
  else if (Array.isArray(parsed)){
    for (i = 0; i < parsed.length && !res; ++i){
      res |= pertenece(variable,parsed[i]);
    }
  }
  return res;
}

function mgu(pairs){//Se asume que pairs es un arreglo de pares de strings, donde cada par es un arreglo de dos strings.
  let res = new Map();
  while (pairs.length > 0){
    let pair = pairs[0];
    if ((!Array.isArray(pair)) || (pair.length != 2)) {
      return "Error: formato incorrecto para el par "+showPair(pair)+".";
    }
    if (equals(pair[0], pair[1])) {//Elim. Lo separo por claridad.
      pairs.shift();
    }
    else {
      if(esVariable(pair[1])&&!esVariable(pair[0])){//Swap. Puedo seguir trabajando con este par.
	pair = swap(pair);
      }
      let izq = pair[0];
      let der = pair[1];
      if (esVariable(izq)){//Resultado o error de occurs check.
	if (pertenece(izq,parsear(der))){
	  return "Falla: occurs check para el par "+showPair(pair)+".";
	}
	//Se actualiza el resultado y el conjunto de pares.
	let subst = new Map([[izq, der]]);
	res = compose(subst, res);
	pairs.shift();
	pairs = pairs.map(function(par) {return new Array(applyS(par[0],subst),applyS(par[1],subst))});
      }
      else{//Ninguno de los dos es variable.
	let parsed1 = parsear(izq);
	let parsed2 = parsear(der);
	if((!esExpr(parsed1))||(!esExpr(parsed2))){//Al menos uno es una constante.
	  if (parsed1 != parsed2){
	    return "Falla: colisi&oacute;n para el par "+showPair(pair)+". Los s&iacute;mbolos no coinciden.";
	  }
	}
	else {//Ambos son expresiones.
	  if (!esExpr(parsed1)){
	    return "Error: "+izq+" no es una expresi&oacute;n v&aacute;lida.";
	  }
	  if (!esExpr(parsed2)){
	    return "Error: "+der+" no es una expresi&oacute;n v&aacute;lida.";
	  }
	  if (parsed1.symbol!=parsed2.symbol){
	    return "Falla: colisi&oacute;n para el par "+showPair(pair)+". Los s&iacute;mbolos no coinciden.";
	  }
	  else {
	    if (parsed1.args.length != parsed2.args.length){
	      return "Falla: colisi&oacute;n para el par "+showPair(pair)+". Las aridades no coinciden.";
	    }
	    pairs = decompose(parsed1.args, parsed2.args, pairs);
	  }
	}
	pairs.shift();
      }
    }
  }
  return res;
}

function mguAll(atoms){
  let pairs = new Array();
  if (esVariable(atoms[0])){
    return "Error: "+atoms[0]+" es una variable, no un literal.";
  }
  for (i = 1; i < atoms.length; ++i){
    pairs.push(new Array(atoms[0], atoms[i]));
    if (esVariable(atoms[i])){
      return "Error: "+atoms[i]+" es una variable, no un literal.";
    }
  }
  return mgu(pairs);
}

function applyStoC(claus,subst){
  return claus.map(function(elem){return applyS(elem, subst);});
}

function fv(parsed){
  let res = new Array();
  if (esExpr(parsed)){
    let content = parsed.args;
    res = nub(concatMap(fv,content));
  }
  else if (esVariable(parsed)){
    res.push(parsed);
  }
  else if (Array.isArray(parsed)){
    res = nub(concatMap(fv,parsed));
  }
  return res;
}

function numerar(variable, number, subset){//toReplace sirve para renombrar solo si tiene un valor determinado.
  let res = variable;
  if ((subset == undefined) || (subset.indexOf(variable) >= 0)) {
    let index = variable.search("[0-9]+");
    if (index >= 0){
      res = variable.substring(0,index);
    }
    res = res + number;
  }
  return res;
}

function renombrar(claus, number, subset){//subset sirve para renombrar solo las variables que estén en un conjunto determinado.
  let renombre = new Map();
  fv(claus.map(parsear)).forEach(function(e){renombre.set(e, numerar(e,number,subset));});
  return claus.map(function(e){return applyS(e,renombre)});
}

function resolventeYSubst(clausPos, clausNeg, litPos, litNeg){
  let res;
  let claus1 = copy(clausPos);
  let claus2 = copy(clausNeg);
  removeIntersection(claus1,litPos);
  removeIntersection(claus2,litNeg);
  let aUnificar = litPos.concat(litNeg.map(function(str){ return str.substring(1, str.length); }));
  let subst = mguAll(aUnificar);
  if (subst instanceof Map){
    let multiset = claus1.concat(claus2);
    multiset = applyStoC(multiset,subst);
    res = {clause: nub(multiset), subst: subst};//Para hacerles show cuando corresponda.
  }
  else res = subst;
  return res;
}
