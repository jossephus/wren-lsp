/* empty css              */(function(){const t=document.createElement("link").relList;if(t&&t.supports&&t.supports("modulepreload"))return;for(const o of document.querySelectorAll('link[rel="modulepreload"]'))r(o);new MutationObserver(o=>{for(const i of o)if(i.type==="childList")for(const a of i.addedNodes)a.tagName==="LINK"&&a.rel==="modulepreload"&&r(a)}).observe(document,{childList:!0,subtree:!0});function n(o){const i={};return o.integrity&&(i.integrity=o.integrity),o.referrerPolicy&&(i.referrerPolicy=o.referrerPolicy),o.crossOrigin==="use-credentials"?i.credentials="include":o.crossOrigin==="anonymous"?i.credentials="omit":i.credentials="same-origin",i}function r(o){if(o.ep)return;o.ep=!0;const i=n(o);fetch(o.href,i)}})();function Z(e,t){(t==null||t>e.length)&&(t=e.length);for(var n=0,r=Array(t);n<t;n++)r[n]=e[n];return r}function ye(e){if(Array.isArray(e))return e}function he(e,t,n){return(t=je(t))in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function be(e,t){var n=e==null?null:typeof Symbol<"u"&&e[Symbol.iterator]||e["@@iterator"];if(n!=null){var r,o,i,a,c=[],u=!0,g=!1;try{if(i=(n=n.call(e)).next,t!==0)for(;!(u=(r=i.call(n)).done)&&(c.push(r.value),c.length!==t);u=!0);}catch(S){g=!0,o=S}finally{try{if(!u&&n.return!=null&&(a=n.return(),Object(a)!==a))return}finally{if(g)throw o}}return c}}function we(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.
In order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}function J(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter(function(o){return Object.getOwnPropertyDescriptor(e,o).enumerable})),n.push.apply(n,r)}return n}function Y(e){for(var t=1;t<arguments.length;t++){var n=arguments[t]!=null?arguments[t]:{};t%2?J(Object(n),!0).forEach(function(r){he(e,r,n[r])}):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):J(Object(n)).forEach(function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(n,r))})}return e}function ve(e,t){if(e==null)return{};var n,r,o=Oe(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)===-1&&{}.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}function Oe(e,t){if(e==null)return{};var n={};for(var r in e)if({}.hasOwnProperty.call(e,r)){if(t.indexOf(r)!==-1)continue;n[r]=e[r]}return n}function Se(e,t){return ye(e)||be(e,t)||Ce(e,t)||we()}function Ee(e,t){if(typeof e!="object"||!e)return e;var n=e[Symbol.toPrimitive];if(n!==void 0){var r=n.call(e,t);if(typeof r!="object")return r;throw new TypeError("@@toPrimitive must return a primitive value.")}return(t==="string"?String:Number)(e)}function je(e){var t=Ee(e,"string");return typeof t=="symbol"?t:t+""}function Ce(e,t){if(e){if(typeof e=="string")return Z(e,t);var n={}.toString.call(e).slice(8,-1);return n==="Object"&&e.constructor&&(n=e.constructor.name),n==="Map"||n==="Set"?Array.from(e):n==="Arguments"||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)?Z(e,t):void 0}}function Pe(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function Q(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter(function(o){return Object.getOwnPropertyDescriptor(e,o).enumerable})),n.push.apply(n,r)}return n}function X(e){for(var t=1;t<arguments.length;t++){var n=arguments[t]!=null?arguments[t]:{};t%2?Q(Object(n),!0).forEach(function(r){Pe(e,r,n[r])}):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):Q(Object(n)).forEach(function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(n,r))})}return e}function Me(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];return function(r){return t.reduceRight(function(o,i){return i(o)},r)}}function E(e){return function t(){for(var n=this,r=arguments.length,o=new Array(r),i=0;i<r;i++)o[i]=arguments[i];return o.length>=e.length?e.apply(this,o):function(){for(var a=arguments.length,c=new Array(a),u=0;u<a;u++)c[u]=arguments[u];return t.apply(n,[].concat(o,c))}}}function D(e){return{}.toString.call(e).includes("Object")}function ke(e){return!Object.keys(e).length}function C(e){return typeof e=="function"}function Ae(e,t){return Object.prototype.hasOwnProperty.call(e,t)}function xe(e,t){return D(t)||p("changeType"),Object.keys(t).some(function(n){return!Ae(e,n)})&&p("changeField"),t}function De(e){C(e)||p("selectorType")}function Le(e){C(e)||D(e)||p("handlerType"),D(e)&&Object.values(e).some(function(t){return!C(t)})&&p("handlersType")}function _e(e){e||p("initialIsRequired"),D(e)||p("initialType"),ke(e)&&p("initialContent")}function Re(e,t){throw new Error(e[t]||e.default)}var Ie={initialIsRequired:"initial state is required",initialType:"initial state should be an object",initialContent:"initial state shouldn't be an empty object",handlerType:"handler should be an object or a function",handlersType:"all handlers should be a functions",selectorType:"selector should be a function",changeType:"provided value of changes should be an object",changeField:'it seams you want to change a field in the state which is not specified in the "initial" state',default:"an unknown error accured in `state-local` package"},p=E(Re)(Ie),x={changes:xe,selector:De,handler:Le,initial:_e};function Te(e){var t=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};x.initial(e),x.handler(t);var n={current:e},r=E(Ke)(n,t),o=E(We)(n),i=E(x.changes)(e),a=E(Ne)(n);function c(){var g=arguments.length>0&&arguments[0]!==void 0?arguments[0]:function(S){return S};return x.selector(g),g(n.current)}function u(g){Me(r,o,i,a)(g)}return[c,u]}function Ne(e,t){return C(t)?t(e.current):t}function We(e,t){return e.current=X(X({},e.current),t),t}function Ke(e,t,n){return C(t)?t(e.current):Object.keys(n).forEach(function(r){var o;return(o=t[r])===null||o===void 0?void 0:o.call(t,e.current[r])}),n}var Be={create:Te},Fe={paths:{vs:"https://cdn.jsdelivr.net/npm/monaco-editor@0.55.1/min/vs"}};function He(e){return function t(){for(var n=this,r=arguments.length,o=new Array(r),i=0;i<r;i++)o[i]=arguments[i];return o.length>=e.length?e.apply(this,o):function(){for(var a=arguments.length,c=new Array(a),u=0;u<a;u++)c[u]=arguments[u];return t.apply(n,[].concat(o,c))}}}function ze(e){return{}.toString.call(e).includes("Object")}function Ue(e){return e||ee("configIsRequired"),ze(e)||ee("configType"),e.urls?($e(),{paths:{vs:e.urls.monacoBase}}):e}function $e(){console.warn(oe.deprecation)}function qe(e,t){throw new Error(e[t]||e.default)}var oe={configIsRequired:"the configuration object is required",configType:"the configuration object should be an object",default:"an unknown error accured in `@monaco-editor/loader` package",deprecation:`Deprecation warning!
    You are using deprecated way of configuration.

    Instead of using
      monaco.config({ urls: { monacoBase: '...' } })
    use
      monaco.config({ paths: { vs: '...' } })

    For more please check the link https://github.com/suren-atoyan/monaco-loader#config
  `},ee=He(qe)(oe),Ve={config:Ue},Ge=function(){for(var t=arguments.length,n=new Array(t),r=0;r<t;r++)n[r]=arguments[r];return function(o){return n.reduceRight(function(i,a){return a(i)},o)}};function ie(e,t){return Object.keys(t).forEach(function(n){t[n]instanceof Object&&e[n]&&Object.assign(t[n],ie(e[n],t[n]))}),Y(Y({},e),t)}var Ze={type:"cancelation",msg:"operation is manually canceled"};function K(e){var t=!1,n=new Promise(function(r,o){e.then(function(i){return t?o(Ze):r(i)}),e.catch(o)});return n.cancel=function(){return t=!0},n}var Je=["monaco"],Ye=Be.create({config:Fe,isInitialized:!1,resolve:null,reject:null,monaco:null}),ae=Se(Ye,2),k=ae[0],I=ae[1];function Qe(e){var t=Ve.config(e),n=t.monaco,r=ve(t,Je);I(function(o){return{config:ie(o.config,r),monaco:n}})}function Xe(){var e=k(function(t){var n=t.monaco,r=t.isInitialized,o=t.resolve;return{monaco:n,isInitialized:r,resolve:o}});if(!e.isInitialized){if(I({isInitialized:!0}),e.monaco)return e.resolve(e.monaco),K(B);if(window.monaco&&window.monaco.editor)return se(window.monaco),e.resolve(window.monaco),K(B);Ge(et,nt)(rt)}return K(B)}function et(e){return document.body.appendChild(e)}function tt(e){var t=document.createElement("script");return e&&(t.src=e),t}function nt(e){var t=k(function(r){var o=r.config,i=r.reject;return{config:o,reject:i}}),n=tt("".concat(t.config.paths.vs,"/loader.js"));return n.onload=function(){return e()},n.onerror=t.reject,n}function rt(){var e=k(function(n){var r=n.config,o=n.resolve,i=n.reject;return{config:r,resolve:o,reject:i}}),t=window.require;t.config(e.config),t(["vs/editor/editor.main"],function(n){var r=n.m||n;se(r),e.resolve(r)},function(n){e.reject(n)})}function se(e){k().monaco||I({monaco:e})}function ot(){return k(function(e){var t=e.monaco;return t})}var B=new Promise(function(e,t){return I({resolve:e,reject:t})}),it={config:Qe,init:Xe,__getMonacoInstance:ot};let L=null,O=!1;const te=new Map,ce=new Map;let at=1,st=1;const H=new Map,h="file:///playground/main.wren";function ct(e){return e===1?"error":e===2?"warning":e===3?"info":"hint"}function ut(e){!e||typeof e.uri!="string"||!Array.isArray(e.diagnostics)||ce.set(e.uri,e.diagnostics.map(t=>({line:t.range.start.line,colStart:t.range.start.character,colEnd:t.range.end.character,severity:ct(t.severity??3),message:t.message??""})))}function lt(e){const t=[];for(const n of e)try{t.push(JSON.parse(n))}catch{}return t}function dt(e){for(const t of e){const n=t;n.method==="textDocument/publishDiagnostics"&&ut(n.params)}}function ue(e){return new Promise((t,n)=>{const r=at++;H.set(r,{resolve:t,reject:n}),L.postMessage({token:r,...e})})}async function le(e){const t=await ue({payload:JSON.stringify(e)}),n=lt(t);return dt(n),n}async function _(e,t){L&&await ue({control:{type:"setVirtualFile",uri:e,content:t}})}async function T(e,t){const n=st++,r=await le({jsonrpc:"2.0",id:n,method:e,params:t});for(const o of r){const i=o;if(i.id===n){if(i.error)throw new Error(i.error.message||"LSP request failed");return i.result}}return null}async function z(e,t){await le({jsonrpc:"2.0",method:e,params:t})}async function q(e,t,n,r,o=h){if(!O)return null;await N(e,o);const i=de(e,t);return await T(n,r(i))}function de(e,t){let n=0,r=0;for(let o=0;o<t&&o<e.length;o++)e[o]===`
`?(n++,r=0):r++;return{line:n,character:r}}function ft(e){return e===7?"class":e===2?"method":e===3?"function":e===6?"variable":e===5?"field":e===14?"keyword":e===1?"text":e===4?"constructor":e===9?"module":e===8?"interface":"variable"}async function N(e,t=h){const n=te.get(t);if(!n){const r={source:e,version:1,opened:!0};te.set(t,r),await z("textDocument/didOpen",{textDocument:{uri:t,languageId:"wren",version:r.version,text:e}});return}e!==n.source&&(n.source=e,n.version+=1,await z("textDocument/didChange",{textDocument:{uri:t,version:n.version},contentChanges:[{text:e}]}))}async function gt(){L=new Worker(new URL("/wren-lsp/assets/lsp-worker-A8Rq6xCE.js",import.meta.url),{type:"module"}),L.onmessage=e=>{const{token:t,outputs:n,error:r}=e.data,o=H.get(t);if(o){if(H.delete(t),r){o.reject(new Error(r));return}o.resolve(n||[])}},await T("initialize",{processId:null,rootUri:null,initializationOptions:null,capabilities:{},workspaceFolders:null}),await z("initialized",{}),O=!0}function b(){return O}async function mt(e,t=h){return O?(await N(e,t),{diagnostics:ce.get(t)||[]}):{diagnostics:[]}}async function pt(e,t,n,r=h){if(!O)return[];await N(e,r);const o=de(e,t),i=typeof n=="string"&&n.length>0,a=await T("textDocument/completion",{textDocument:{uri:r},position:o,context:{triggerKind:i?2:1,triggerCharacter:i?n:void 0}});return a?(Array.isArray(a)?a:a.items||[]).map(u=>({label:u.label,kind:ft(u.kind)})):[]}async function yt(e,t,n=h){return await q(e,t,"textDocument/definition",r=>({textDocument:{uri:n},position:r}),n)}async function ht(e,t,n=h){return await q(e,t,"textDocument/references",r=>({textDocument:{uri:n},position:r,context:{includeDeclaration:!0}}),n)}async function bt(e,t,n=h){const r=await q(e,t,"textDocument/hover",o=>({textDocument:{uri:n},position:o}),n);return console.log("[playground] hover response",r),r}async function wt(e,t=h){return O?(await N(e,t),await T("textDocument/documentSymbol",{textDocument:{uri:t}})):null}const vt={keywords:["break","class","construct","continue","else","false","for","foreign","if","import","in","is","null","return","static","super","this","true","var","while"],builtins:["Bool","Class","Fiber","Fn","List","Map","Null","Num","Object","Range","Sequence","String","System"],tokenizer:{root:[[/\/\*/,"comment","@comment"],[/\/\/.*$/,"comment"],[/"/,"string","@string"],[/0x[0-9a-fA-F]+/,"number"],[/\d+(\.\d+)?([eE][+-]?\d+)?/,"number"],[/__[a-zA-Z_]\w*/,"variable.field"],[/_[a-zA-Z_]\w*/,"variable.field"],[/[A-Z]\w*/,{cases:{"@builtins":"type","@default":"type"}}],[/[a-z_]\w*/,{cases:{"@keywords":"keyword","@default":"identifier"}}],[/[+\-*\/%&|^~<>=!]+/,"operator"],[/[{}()\[\]]/,"@brackets"],[/\s+/,"white"]],comment:[[/\/\*/,"comment","@push"],[/\*\//,"comment","@pop"],[/./,"comment"]],string:[[/\\./,"string.escape"],[/"/,"string","@pop"],[/./,"string"]]}},Ot={comments:{lineComment:"//",blockComment:["/*","*/"]},brackets:[["{","}"],["[","]"],["(",")"]],autoClosingPairs:[{open:"{",close:"}"},{open:"[",close:"]"},{open:"(",close:")"},{open:'"',close:'"'}],surroundingPairs:[{open:"{",close:"}"},{open:"[",close:"]"},{open:"(",close:")"},{open:'"',close:'"'}]};let s;const fe=new Set(["main.wren","dome.wren","wren-lsp.json","resolver.js"]),l=[{id:"main.wren",uri:"file:///playground/main.wren",language:"wren",content:` 
System.print("Hello World")
// a = 1 // un comment this to see lsp error

`},{id:"dome.wren",uri:"file:///playground/dome.wren",language:"wren",content:`// The below config files wren-lsp.json and the custom resolver.js enable
// us to import declaring our own resolving mechanisms.
// resolver.js uses github link for dome to resolve files
// This works fine because wren-lsp configuration is configurable host wise (although we are doing a little bit too much for the wasm, this is in order to show the capability)

import "input" for Keyboard
import "graphics" for Canvas, Color

class Main {
  construct new() {}

  init() {
    _x = 10
    _y = 10
    _w = 5
    _h = 5
  }

  update() {
    if (Keyboard.isKeyDown("left")) {
      _x = _x - 1
    }
    if (Keyboard.isKeyDown("right")) {
      _x = _x+ 1
    }
    if (Keyboard.isKeyDown("up")) {
      _y = _y - 1
    }
    if (Keyboard.isKeyDown("down")) {
      _y = _y + 1
    }
  }

  draw(alpha) {
    Canvas.cls()
    var color = Color.rgb(171, 82, 54)
    Canvas.rectfill(_x, _y, _w, _h, color)
  }
}

var Game = Main.new()
`},{id:"wren-lsp.json",uri:"file:///playground/wren-lsp.json",language:"json",content:`{
  "version": 1,
  "resolvers": [
    {
      "type": "host",
      "script": "./resolver.js",
      "fallbackToBuiltin": true
    },
    {
      "type": "path",
      "roots": ["./"],
      "delimiter": "/"
    }
  ]
}`},{id:"resolver.js",uri:"file:///playground/resolver.js",language:"javascript",content:`const REPO_OWNER = "domeengine"
const REPO_NAME = "dome"
const REPO_REF = "main"
const REPO_API_BASE = "https://api.github.com/repos/" + REPO_OWNER + "/" + REPO_NAME
const REPO_RAW_BASE = "https://raw.githubusercontent.com/" + REPO_OWNER + "/" + REPO_NAME + "/" + REPO_REF
const MODULE_ROOTS = ["src/modules", "src/bindings", "src"]

const DOME_SCHEME = "dome://"
const domeModules = new Map()

const stripWrenExtension = (path) => path.endsWith(".wren") ? path.slice(0, -5) : path

const normalizePath = (path) => {
  const out = []
  for (const part of path.split("/")) {
    if (!part || part == ".") continue
    if (part == "..") {
      if (out.length > 0) out.pop()
      continue
    }
    out.push(part)
  }
  return out.join("/")
}

const candidateKeys = (importString) => {
  const raw = stripWrenExtension(importString)
  const slash = raw.replaceAll(".", "/")
  const keys = new Set([raw, slash])
  for (const root of MODULE_ROOTS) {
    keys.add(root + "/" + raw)
    keys.add(root + "/" + slash)
  }
  if (raw.startsWith("dome/")) keys.add(raw.slice("dome/".length))
  if (slash.startsWith("dome/")) keys.add(slash.slice("dome/".length))
  return [...keys].map(normalizePath).filter((k) => k.length > 0)
}

export async function preloadDomeModules() {
  if (domeModules.size > 0) return
  const treeUrl = REPO_API_BASE + "/git/trees/" + REPO_REF + "?recursive=1"
  const treeResp = await fetch(treeUrl)
  if (!treeResp.ok) return
  const tree = await treeResp.json()

  const files = (tree.tree || [])
    .filter((entry) => entry.type == "blob" && entry.path.endsWith(".wren"))
    .map((entry) => entry.path)

  await Promise.all(files.map(async (path) => {
    const rawUrl = REPO_RAW_BASE + "/" + path
    const resp = await fetch(rawUrl)
    if (!resp.ok) return
    const source = await resp.text()
    const key = stripWrenExtension(path)
    const uri = DOME_SCHEME + key
    domeModules.set(key, {
      canonical_id: uri,
      uri,
      source,
      kind: "virtual",
    })
  }))
}

export function resolveHostImport(importerUri, importString) {
  if ((importString.startsWith("./") || importString.startsWith("../")) && importerUri.startsWith(DOME_SCHEME)) {
    const importerPath = importerUri.slice(DOME_SCHEME.length)
    const slash = importerPath.lastIndexOf("/")
    const baseDir = slash >= 0 ? importerPath.slice(0, slash) : ""
    const relative = normalizePath(baseDir + "/" + stripWrenExtension(importString))
    return domeModules.get(relative) || null
  }

  for (const key of candidateKeys(importString)) {
    const module = domeModules.get(key)
    if (module) return module
  }
  return null
}`}];let y=l[0].id,d=null;const v=new Map;let R=null,St=0;const U=new Map;function Et(){R=new Worker(new URL("/wren-lsp/assets/run-worker-BqcAP3Lg.js",import.meta.url),{type:"module"}),R.onmessage=e=>{const{token:t,output:n,errors:r,result:o}=e.data,i=U.get(t);i&&(U.delete(t),i.resolve({output:n,errors:r,result:o}))}}function jt(e){return R||Et(),new Promise(t=>{const n=++St;U.set(n,{resolve:t}),R.postMessage({token:n,source:e})})}const P=document.getElementById("run-btn"),$=document.getElementById("output");function Ct(e,t){let n="";e&&(n+='<span class="output-text">'+M(e)+"</span>"),t&&(n+='<span class="output-error">'+M(t)+"</span>"),!e&&!t&&(n='<span class="output-hint">No output</span>'),$.innerHTML=n}async function ge(){if(!(!d||!A())){P.disabled=!0,$.innerHTML='<span class="output-running">Running…</span>';try{const{output:e,errors:t}=await jt(d.getValue());Ct(e,t)}catch(e){$.innerHTML='<span class="output-error">'+M(e.message)+"</span>"}finally{P.disabled=!1}}}P.addEventListener("click",ge);function f(){return l.find(e=>e.id===y)||l[0]}function A(){return f().language==="wren"}function Pt(){s.editor.defineTheme("wren-dark",{base:"vs-dark",inherit:!0,rules:[{token:"keyword",foreground:"c586c0"},{token:"type",foreground:"4ec9b0"},{token:"identifier",foreground:"9cdcfe"},{token:"string",foreground:"ce9178"},{token:"string.escape",foreground:"d7ba7d"},{token:"number",foreground:"b5cea8"},{token:"comment",foreground:"6a9955"},{token:"operator",foreground:"d4d4d4"},{token:"variable.field",foreground:"9cdcfe",fontStyle:"italic"}],colors:{"editor.background":"#111111","editor.foreground":"#e0e0e0","editorGutter.background":"#0d0d0d","editorLineNumber.foreground":"#404040","editorLineNumber.activeForeground":"#808080","editor.lineHighlightBackground":"#1a1a1a","editor.selectionBackground":"#264f78","editorCursor.foreground":"#e0e0e0","editorWidget.background":"#1a1a1a","editorWidget.border":"#333333","editorSuggestWidget.background":"#1a1a1a","editorSuggestWidget.border":"#333333","editorSuggestWidget.selectedBackground":"#264f78","editorHoverWidget.background":"#1a1a1a","editorHoverWidget.border":"#333333"}})}const F=document.getElementById("diagnostics"),ne=document.getElementById("status");function j(e,t){ne.textContent=e,ne.className=t?"status status-ok":"status"}function M(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")}function re(e){if(!A()){F.innerHTML='<span class="diag-hint">Diagnostics available for .wren files only</span>',j("Viewing non-Wren file",!0);return}if(!e||e.length===0){F.innerHTML='<span class="diag-hint">No issues found ✓</span>',j("No issues",!0);return}j(e.length+" issue"+(e.length!==1?"s":""),!1),F.innerHTML=e.map(t=>{const n="diag-"+t.severity;return'<div class="diag"><span class="diag-location">'+(t.line+1+":"+t.colStart)+'</span> <span class="'+n+'">'+M(t.severity)+": "+M(t.message)+"</span></div>"}).join("")}async function W(){if(!b()||!d)return;if(!A()){s.editor.setModelMarkers(d.getModel(),"wren-lsp",[]),re([]);return}const e=d.getValue(),n=(await mt(e,f().uri)).diagnostics,r=n.map(o=>({severity:o.severity==="error"?s.MarkerSeverity.Error:o.severity==="warning"?s.MarkerSeverity.Warning:o.severity==="info"?s.MarkerSeverity.Info:s.MarkerSeverity.Hint,message:o.message,startLineNumber:o.line+1,startColumn:o.colStart+1,endLineNumber:o.line+1,endColumn:o.colEnd+1}));s.editor.setModelMarkers(d.getModel(),"wren-lsp",r),re(n)}function Mt(e){const t=r=>{if(!r||typeof r!="object"||Array.isArray(r))return r;const o=Object.keys(r);return o.length!==1?r:r[o[0]]},n=r=>{const o=t(r);return o?typeof o=="string"?o:Array.isArray(o)?o.map(n).filter(Boolean).join(`
`):typeof o=="object"?typeof o.value=="string"||typeof o.kind=="string"&&typeof o.value=="string"?o.value:n(t(o)):"":""};return e?n(e):""}function kt(e){return e?Array.isArray(e)?e.length>0?e[0]:null:e.uri&&e.range?e:Array.isArray(e.targets)&&e.targets.length>0?{uri:e.targets[0].uri,range:e.targets[0].targetSelectionRange||e.targets[0].targetRange}:null:null}function At(){s.languages.registerCompletionItemProvider("wren",{triggerCharacters:["."],provideCompletionItems:async(e,t)=>{if(!b())return{suggestions:[]};const n=e.getValue(),r=e.getOffsetAt(t),o=e.getWordUntilPosition(t),c=e.getLineContent(t.lineNumber)[o.startColumn-2]===".",S=(await pt(n,r,c?".":void 0,f().uri)).filter(m=>c?m.kind==="method"||m.kind==="function":m.kind!=="method"&&m.kind!=="function"),me={startLineNumber:t.lineNumber,startColumn:o.startColumn,endLineNumber:t.lineNumber,endColumn:o.endColumn},pe={class:s.languages.CompletionItemKind.Class,method:s.languages.CompletionItemKind.Method,function:s.languages.CompletionItemKind.Function,variable:s.languages.CompletionItemKind.Variable,field:s.languages.CompletionItemKind.Field,keyword:s.languages.CompletionItemKind.Keyword,text:s.languages.CompletionItemKind.Text,constructor:s.languages.CompletionItemKind.Constructor,module:s.languages.CompletionItemKind.Module,interface:s.languages.CompletionItemKind.Interface};return{suggestions:S.map(m=>({label:m.label,kind:pe[m.kind]||s.languages.CompletionItemKind.Text,insertText:m.label,range:me}))}}}),s.languages.registerHoverProvider("wren",{provideHover:async(e,t)=>{if(!b())return null;const n=e.getValue(),r=e.getOffsetAt(t),o=await bt(n,r,f().uri),i=Mt(o?.contents);if(!i)return null;const a=e.getWordAtPosition(t);return{range:a?{startLineNumber:t.lineNumber,startColumn:a.startColumn,endLineNumber:t.lineNumber,endColumn:a.endColumn}:void 0,contents:[{value:"```\n"+i+"\n```"}]}}}),s.languages.registerDefinitionProvider("wren",{provideDefinition:async(e,t)=>{if(!b())return null;const n=e.getValue(),r=e.getOffsetAt(t),o=await yt(n,r,f().uri),i=kt(o);return!i||i.uri!==f().uri?null:{uri:e.uri,range:{startLineNumber:i.range.start.line+1,startColumn:i.range.start.character+1,endLineNumber:i.range.end.line+1,endColumn:i.range.end.character+1}}}}),s.languages.registerReferenceProvider("wren",{provideReferences:async(e,t)=>{if(!b())return[];const n=e.getValue(),r=e.getOffsetAt(t),o=await ht(n,r,f().uri);return(Array.isArray(o)?o:[]).filter(a=>a.uri===f().uri).map(a=>({uri:e.uri,range:{startLineNumber:a.range.start.line+1,startColumn:a.range.start.character+1,endLineNumber:a.range.end.line+1,endColumn:a.range.end.character+1}}))}}),s.languages.registerDocumentSymbolProvider("wren",{provideDocumentSymbols:async e=>{if(!b())return[];const t=e.getValue(),n=await wt(t,f().uri),r=Array.isArray(n)?n:[],o={5:s.languages.SymbolKind.Class,6:s.languages.SymbolKind.Method,12:s.languages.SymbolKind.Function,13:s.languages.SymbolKind.Variable};return r.map(i=>({name:i.name,kind:o[i.kind]||s.languages.SymbolKind.Variable,range:{startLineNumber:(i.range?.start?.line??0)+1,startColumn:(i.range?.start?.character??0)+1,endLineNumber:(i.range?.end?.line??0)+1,endColumn:(i.range?.end?.character??0)+1},selectionRange:{startLineNumber:(i.selectionRange?.start?.line??i.range?.start?.line??0)+1,startColumn:(i.selectionRange?.start?.character??i.range?.start?.character??0)+1,endLineNumber:(i.selectionRange?.end?.line??i.range?.end?.line??0)+1,endColumn:(i.selectionRange?.end?.character??i.range?.end?.character??0)+1}}))}})}function xt(){for(const e of l){const t=s.Uri.parse(e.uri),n=s.editor.createModel(e.content,e.language,t);v.set(e.id,n),n.onDidChangeContent(()=>{e.content=n.getValue(),_(e.uri,e.content),e.id===y&&(w&&clearTimeout(w),w=setTimeout(()=>W(),300))})}}let w=null;async function Dt(e){if(e.endsWith(".wren")||(e+=".wren"),l.some(o=>o.id===e))return;const t={id:e,uri:"file:///playground/"+e,language:"wren",content:""};l.push(t),await _(t.uri,t.content);const n=s.Uri.parse(t.uri),r=s.editor.createModel(t.content,t.language,n);v.set(t.id,r),r.onDidChangeContent(()=>{t.content=r.getValue(),_(t.uri,t.content),t.id===y&&(w&&clearTimeout(w),w=setTimeout(()=>W(),300))}),G(t.id)}function Lt(e){if(fe.has(e))return;const t=l.findIndex(r=>r.id===e);if(t<0)return;l.splice(t,1);const n=v.get(e);n&&(n.dispose(),v.delete(e)),y===e?G(l[0].id):V()}function _t(){const e=prompt("File name (e.g. utils.wren):");!e||!e.trim()||Dt(e.trim())}const Rt=new Set(["wren-lsp.json","resolver.js"]);function V(){const e=document.getElementById("tabs-bar"),t=document.getElementById("config-tabs-bar"),n=document.getElementById("tab-add-btn"),r=e.querySelectorAll(".tab");for(let o=0;o<r.length;o++)r[o].remove();t.innerHTML="";for(const o of l){const i=Rt.has(o.id),a=document.createElement("button");if(a.className="tab"+(o.id===y?" active":""),a.textContent=o.id,a.addEventListener("click",()=>G(o.id)),!fe.has(o.id)){const c=document.createElement("span");c.className="tab-close",c.textContent="×",c.title="Delete "+o.id,c.addEventListener("click",u=>{u.stopPropagation(),Lt(o.id)}),a.appendChild(c)}i?t.appendChild(a):e.insertBefore(a,n)}}async function G(e){!d||e===y||!l.find(n=>n.id===e)||(y=e,d.setModel(v.get(e)),P.disabled=!A(),V(),await W())}async function It(){s=await it.init(),s.languages.register({id:"wren"}),s.languages.setMonarchTokensProvider("wren",vt),s.languages.setLanguageConfiguration("wren",Ot),Pt(),xt(),At(),d=s.editor.create(document.getElementById("editor"),{model:v.get(y),theme:"wren-dark",minimap:{enabled:!1},scrollBeyondLastLine:!1,fontSize:13,fontFamily:'ui-monospace, "SF Mono", Menlo, monospace',lineHeight:20,automaticLayout:!0,tabSize:2,renderLineHighlight:"line",scrollbar:{verticalScrollbarSize:8,horizontalScrollbarSize:8}}),d.addAction({id:"wren-run",label:"Run Wren Code",keybindings:[s.KeyMod.CtrlCmd|s.KeyCode.Enter],run:ge}),document.getElementById("tab-add-btn").addEventListener("click",_t);try{await gt();for(const t of l)await _(t.uri,t.content);P.disabled=!A(),j("Ready",!0)}catch(t){j("Failed to load WASM: "+t.message,!1),console.error(t)}V(),await W()}It();
