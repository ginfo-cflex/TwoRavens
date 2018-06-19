import m from 'mithril'

import Search from '../../../app/views/Search';

import {varColor, mergeAttributes, menuColor} from "../common";

import Panel from './Panel';
import MenuTabbed from './MenuTabbed';
import * as common from "../common";
import * as index from "../../../app/index";
import * as app from "../../../app/app";

import {
    borderColor,
    heightHeader,
    heightFooter,
    panelMargin,
    canvasScroll,
    scrollbarWidth,
} from "../common";


let varList = [];
let currentVal = "variable" ;
let config = '';
let configName = '';
let dataDetails = {};

let peekSkip = 0;
let peekData = [];

let tableData = [];
let tableHeader = [];

var filterIndex =[];
var filterVars =[];

var operations = ['sqrt()','abs()','exp()','log()','log10()','factorial()','ceiling()','floor()'];

export default class Recode {
    oncreate() {
        window.addEventListener('storage', (e) => onRecodeStorageEvent(this, e));
    }

    oninit() {

        this.configuration = localStorage.getItem('configuration');
        this.configName = localStorage.getItem('configName');
        config = this.configuration;
        configName = this.configName
        //data/ traindata.csv
        m.request(config).then(data => {
            
            Object.assign(dataDetails,  data['variables']);

            for(var variable in dataDetails){
                varList.push(variable);  
            }
        });
        

        m.request(`rook-custom/rook-files/${configName}/data/trainData.tsv`, {
            deserialize: x => x.split('\n').map(y => y.split('\t'))
        }).then(data => {
            console.log(data.length)
            // simulate only loading some of the data... by just deleting all the other data
            let headers = data[0].map(x => x.replace(/"/g, ''));
            let newData = data.slice(1, data.length-1);
       
            peekData = peekData.concat(newData);
            
            for(var val in peekData){
                tableData.push(peekData[val]);  
            }
            for(var val in headers){
                tableHeader.push(headers[val]);  
            }
        });
    }
    view() {
        return [
            m('nav.navbar.navbar-default',
                [
                    m('div.container-fluid',[
                        m('ul.nav.navbar-nav',[
                            m('li',{id:'createLink'},
                            m('a',{oncreate: createCreate, onclick: createClick},  "Create New Variable")),
                            m('li',{id:'formulaLink'},
                            m('a',{oncreate: formulaCreate, onclick: formulaClick}, "Formula Builder")),
                            m('li',{id:'recodeLink'},
                            m('a', {oncreate: recodeCreate, onclick: recodeClick}, "Recode")),
                            m('li',{id:'reorderLink'},
                            m('a', {oncreate: reorderCreate, onclick: reorderClick}, "Reorder")),
                        ]),
                    ])
                ]),
                m('div.outter',{style:{'padding':'10px'}},[
                    m(Panel, {
                        side: 'left',
                        label: 'Data Selection',
                        hover: true,
                        width: app.modelLeftPanelWidths[app.leftTab],
                        attrsAll: {style: {'z-index': 101, 'overflow':'scroll'}}
                    },
                    m(MenuTabbed,{
                        id: 'leftpanelMenu',
                        attrsAll: {style: {height: 'calc(100% - 39px)'}},
                        currentTab: app.leftTab,
                        callback: app.setLeftTab,
                        sections: [
                            {
                                value: 'Variables',
                                title: '',
                                contents: [
                                    m(`div#varList`, varList.map((item) =>
                                    m(`div#${'varList' + item.replace(/\W/g, '_')}`, mergeAttributes({
                                            style: {
                                                'margin-top': '5px',
                                                'text-align': "center",
                                                'background-color': app.hexToRgba("#FA8072")
                                            },
                                            class: 'var',
                                            onclick: clickVar
                                        }), item)))

                                ]
                            },
                            {
                                value: 'Operations',
                                contents:[
                                    m(`div#operation`, operations.map((item) =>
                                    m(`div#${'operation' + item.replace(/\W/g, '')}`, mergeAttributes({
                                            style: {
                                                'margin-top': '5px',
                                                'text-align': "center",
                                                'background-color': app.hexToRgba("#FA8072")
                                            },
                                            class: 'var',
                                            onclick: addOperations
                                        }), item)))
                                ]
                            }
                        ]
                    }),
                ),
                m(`#rightpanel.container.sidepanel.clearfix`, mergeAttributes({
                    style: {
                        overflow: 'scroll',
                        background: menuColor,
                        border: borderColor,
                        width: (window.innerWidth - 300)+'px' ,
                        height: `calc(100% - ${heightHeader + heightFooter}px - ${2 * panelMargin}px - ${canvasScroll['horizontal'] ? scrollbarWidth : 0}px)`,
                        position: 'fixed',
                        top: heightHeader + panelMargin  + 'px',
                    }}),
                    m('div.container-fluid ',{id: 'recodeDiv'},[
        
                        m('div.container-fluid', {id : 'div1_recode' , style : {'height': '220px','padding':'20px'}}, [
                            m('form',{ onsubmit: calculate},[
                                m('div',{style :{'display': 'block','overflow': 'hidden'}},[
                                    m('input[type=text]', {id: 'variable', placeholder: 'Variable', style : {'width': '100%','box-sizing':'border-box'}}),
                                    m('span',{ onmouseover:_=> showTooltip() , onmouseout:_=> hideTooltip(),style: {'position': 'absolute','display': 'inline','top': '25px','color': 'grey','padding-left': '5px', 'font-size':'17px'}},"?"),
                                    m('div',{id:'tooltip', style: 'right:0; display: none; margin-right: 35px'},[
                                        m('table',[
                                            m('tr',[
                                                m('th','Use..'),
                                                m('th','to do...'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'sqrt('+currentVal+')'),
                                                m('td','square root of a variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'abs('+currentVal+')'),
                                                m('td','absolute value of the variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'exp('+currentVal+')'),
                                                m('td','exponential of the variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'log('+currentVal+')'),
                                                m('td','logarithmic value of a variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'log10('+currentVal+')'),
                                                m('td','logarithmic base 10 value of a variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'factorial('+currentVal+')'),
                                                m('td','factorial of a variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'ceiling('+currentVal+')'),
                                                m('td','ceiling value of a variable'),
                                            ]),
                                            m('tr',[
                                                m('td',{style:{'font-family': 'Courier New'}},'floor('+currentVal+')'),
                                                m('td','floor value of a variable'),
                                            ]),
                                        ])
                                    ]),                            
                                ]),
                                m("br"),
                                m('button[type="submit"]',{style: {'float':'right'}}, 'Customize'),
                            ]),]),
                            m('div.container-fluid', {id : 'div2' , style : {'display':'block','height': '250px','padding':'20px'}}, [
                            m('form',{ onsubmit: calculate2},[
                                m('span',{style :{'display': 'block','overflow': 'hidden'}},[
                                    m('input[type=text]', {id: 'value1', placeholder: 'value 1', style : {'display':'inline-block' , 'margin-right':'10px', 'width':'20%'}}),
                                    m('h3',{ style : {'display':'inline-block', 'margin-right':'10px'}},'-'),
                                    m('input[type=text]', {id: 'value2', placeholder: 'value 1', style : {'display':'inline-block' , 'margin-right':'10px', 'width':'20%'}}),
                                    m('h3',{ style : {'display':'inline-block', 'margin-right':'10px'}},'='),
                                    m('input[type=text]', {id: 'newValue', placeholder: 'New Value', style : {'display':'inline-block' , 'width':'20%'}}),
                                ]),
                                
                                m("br"),
                                m('button[type="submit"]',{style: {'float': 'right'}} ,'Customize'),
                            ])
                        ])
                    
                    ]),
                    m('div.container-fluid', {id: 'formulaDiv'},[
        
                        m('div.container-fluid', {id : 'div1' , style : {'display':'block','height': '220px','padding':'20px'}}, [
                            m('form',{ onsubmit: formulaCalculate},[
                                m('span',{style :{'display': 'block','overflow': 'hidden'}},[m('input[type=text]', {id: 'variables', placeholder: 'Variable', style : {'width': '100%','box-sizing':'border-box','white-space': 'nowrap;'}}),]),
                                m("br"),
                                m('button[type="submit"]',{style: {'float':'right'}}, 'Customize'),
                            ]),])
                    ])
                ),
                m('#centralPanel.container.sidepanel.clearfix', mergeAttributes({
                    style: {
                        overflow: 'scroll',
                        background: menuColor,
                        border: borderColor,
                        width: (window.innerWidth-25)+'px' ,
                        height: `calc(100% - ${heightHeader + heightFooter}px - ${2 * panelMargin}px - ${canvasScroll['horizontal'] ? scrollbarWidth : 0}px)`,
                        position: 'fixed',
                        display:'block',
                        top: heightHeader + panelMargin  + 'px',
                        
                    }}),[
                        // m('div.container', {id : 'createDiv' , style : {'overflow':'scroll','width': '100%','height':'100%'}}, [
                            m('div',{id:'filterTableDiv'},[
                                m('form#selectVars',{onsubmit: filterTable},[
                                    m('table.table.table-bordered',{id:'tableVars'},[
                                        m('tr#colheader',[
                                            m('td.col-xs-4',{style : {'border': '1px solid #ddd','text-align': 'center','font-weight': 'bold'}},'Variables'),
                                            m('td.col-xs-4',{style : {'border': '1px solid #ddd','text-align': 'center','font-weight': 'bold'}},'Checked')
                                        ]),
                                        varList.filter((varItem,j) => j !== 0 ).map((varItem,j)=>m('tr',[
                                            m('td',{style : {'border': '1px solid #ddd','text-align': 'center'}},m('span',varItem)),
                                            m('td',{style : {'border': '1px solid #ddd','text-align': 'center'}},m('input[type=checkbox]',{id:'filterVal'+j,value:varItem+","+j})),
                                            
                                        ])),
                                    ]),
                                    m('button[type="button"]',{onclick: selectAllVars},'Select All Variables'),
                                    m('button[type="submit"]','Filter Table'),
                                    m('button[type="button"]',{onclick: unselectAllVars},'Unselect All Variables'),
                                ]),
                            ]),
                            m('div',{id:'createTableDiv'},[
                                m('form#createNewForm',{ onsubmit: addValue},[
                                    m('div',{style :{'float':'left'  ,'overflow':'hidden','width':'40%'}},[
                                        m('br'),
                                        m('label',{ style : {'display':'inline-block', 'margin-right':'10px'}},'Variable Name : '),
                                        m('input[type=text]', {id: 'newVar', placeholder: 'New Variable', style : {'display':'inline-block' , 'margin':'10px', 'width':'40%'}}),
                                        m('br'),
                                        m('label',{ style : {'display':'inline-block', 'margin-right':'10px'}},'Variable Type : '),
                                        m('div.dropdown',{ style : {'display':'inline-block','margin':'10px'}},[
                                            m('select.form-control#typeSelect',{onchange: someFunction ,style:{'display':'inline-block'}, id:'varType'},[
                                                m('option','Boolean'),
                                                m('option','Nominal'),
                                                m('option','Numchar')
                                            ])
                                        ]),
                                        m('label',{ style : {'display':'block', 'margin-right':'10px'}},'Variable Description : '),
                                        m('textarea',{id:'varDescription',cols:"40",rows:'5'}),
                                        m('div#nominalDiv',{style:{'margin':'10px'}},[
                                            m('label',{ style : { 'margin-right':'10px'}},'Class List : '),
                                            m('input[type=text]',{id:'nominalList', placeholder:'Nominal', style:{'margin':'10px'}},),
                                        ]),
                                    ]),m('div#tableDiv',{style :{'display': 'block','width':'60%','float':'right','height':(window.innerHeight-150)+'px' ,'overflow-y': 'auto','border-style': 'solid','border-width': 'thin'}},[
                                        m('table.table.table-bordered',{id:'createTable',style:{ 'overflow': 'scroll'}},[
                                            m('tr#colheader',[
                                                (tableHeader.slice(1)).filter((header)=> filterVars.includes(header)).map((header) => m('th.col-xs-4',{style : {'border': '1px solid #ddd','text-align': 'center'}},header)),
                                                
                                            ]),
                                            tableData.map((row,i)=> m('tr#colValues',[
                                                row.filter((item,j) => filterIndex.indexOf(j)>=0 ).map((item,j) => m('td',{style : {'border': '1px solid #ddd','text-align': 'center'}},item)),
                                                
                                            ],
                                                
                                            ))
                                        ])                                                                        
                                    ]),
                                    
                                    m("br"),
                                    m('button[type="button"]' ,{onclick: createNewCalculate,style:"float:left;"},'Create New Variable'),
                                    m('button[type="submit"]',{id:'submitButton'},'Add Values'),
                                ])
                            ]),
                            
                        // ])
                    ])
                ]),
            ]}
        }

function onRecodeStorageEvent(recode, e){
    recode.configuration = localStorage.getItem('configuration');
    recode.configName = localStorage.getItem('configName');
    m.redraw();
}

function someFunction(elem){
    if(elem.target[1].selected){
        var elem = document.getElementById('nominalDiv');
        elem.style.visibility="visible";
    }else{
        var elem = document.getElementById('nominalDiv');
        elem.style.visibility="hidden";
    }
}

function selectAllVars(){
    $(':checkbox').prop("checked", true);
}

function unselectAllVars(){
    $(':checkbox').prop("checked", false);
}

function addOperations(elem){
    var text = document.getElementById('variable');
    text.value = this.textContent.split("(")[0]+"("+currentVal+")";
}

function clickVar(elem) {

    if(document.getElementById('recodeLink').className === 'active'){
      
        //If the variable is a of the type 'character', cannot apply mathematical transformations on it.
        if(dataDetails[this.textContent]['numchar'] != 'numeric'){
            var transform =  document.getElementById('div1_recode');
            transform.style.display = 'none';
        }
        else {
            var transform =  document.getElementById('div1_recode');
            transform.style.display = 'block';
            // var text = document.getElementById('variable');
            // text.value = this.textContent;
            currentVal = this.textContent;  
        }
    }

    if(document.getElementById('formulaLink').className === 'active'){
        //Only those variables of type "numeric" could be used to build formula
        if(dataDetails[this.textContent]['numchar'] === 'numeric'){
            var text = document.getElementById('variables');
            text.value = text.value + " " + this.textContent;
        }
     }
     
}
function addValue(elem){
    console.log('Values Added')
    console.log(elem.target[3].value)
    //selectedOptions
    var newVarValue = [];

    if(document.getElementById('typeSelect').selectedIndex === 0){
        for(var i =0;i<tableData.length;i++){
            if(elem.target[2+i].checked){
                newVarValue.push({row:i,value:'yes'})
            }else{
                newVarValue.push({row:i,value:'no'})
            }
        }
    }else if(document.getElementById('typeSelect').selectedIndex === 1){
        for(var i =0;i<tableData.length;i++){
            newVarValue.push({row:i,value:elem.target[3+i].value})
        }
    }else if(document.getElementById('typeSelect').selectedIndex === 2){
        for(var i =0;i<tableData.length;i++){
            newVarValue.push({row:i,value:elem.target[2+i].value})
        }
    }
    
    console.log(newVarValue);   
    console.log(n)
}

function filterTable(elem){
    console.log($("input[type=checkbox]:checked").size());

    if($("input[type=checkbox]:checked").size()<1){
        alert('Select atleast one variable')
    }
    else{
        for(var i =0; i< varList.length-1;i++){
            if(elem.target[i].checked){
                filterIndex.push(++(elem.target[i].value.split(',')[1]));
                filterVars.push(elem.target[i].value.split(',')[0]);
            }
        }
    
        document.getElementById("createTableDiv").setAttribute("style", "display:block");
        document.getElementById("filterTableDiv").setAttribute("style", "display:none");
    }
}

function formulaCalculate(){}

function showTooltip(){
    var tooltip = document.getElementById('tooltip');
    tooltip.style.display = 'block';
    tooltip.style.position ='absolute';
    tooltip.style.whiteSpace = 'nowrap';
    tooltip.style.border = '1px solid black';
    tooltip.style.borderRadius = '5px';
    tooltip.style.padding= "5px";
    tooltip.style.zIndex= "1";
    tooltip.style.backgroundColor= 'white';
    
}
function hideTooltip(){
    var tooltip = document.getElementById('tooltip');
    tooltip.style.display = 'none';
}

function createNewCalculate(){
    console.log(document.getElementById('typeSelect').selectedIndex)
    if(document.getElementById('newVar').value===""){
        alert("Enter Variable Name!");
    }
    else{
        var iter = 0;
    //createNewForm
    $('#createNewForm').find('tr').each(function(){
        var trow = $(this);

        if(iter == 0){
            trow.append('<th class = "col-xs-4" style ="border: 1px solid #ddd; text-align: center;">'+document.getElementById('newVar').value+'</th>');
            iter++;
        }else{
            if(document.getElementById('typeSelect').selectedIndex === 0){
                trow.append('<td style ="border: 1px solid #ddd;text-align: center;"><input type="checkbox" name="newVarVal" id ="newVarVal'+(iter-1)+'" value = 1 /></td>');
            }else if(document.getElementById('typeSelect').selectedIndex === 1){

                var classList = document.getElementById('nominalList').value.split(",");
                trow.append('<td style ="border: 1px solid #ddd;"><select id ="newVarVal'+(iter-1)+'"></select></td>');
                
                var selectBox = document.getElementById("newVarVal"+(iter-1))
                for(var i =0 ;i<classList.length;i++){
                    selectBox.options.add(new Option(classList[i],classList[i]))
                }

            }else if(document.getElementById('typeSelect').selectedIndex === 2){
                trow.append('<td style ="border: 1px solid #ddd;text-align: center;"><input type="text" name="newVarVal" style = "width:100%" id ="newVarVal'+(iter-1)+'"/></td>');
            }
            
            iter++
        }
        
    });
    var elem = document.getElementById('submitButton');
    elem.style.visibility="visible";


    }
    $('#tableDiv').animate({scrollLeft:'+=1500'},500);
    
}
function calculate2(elem){
    var json = {
        'configuration': config,
        'variable': currentVal,
        'start': elem.target[0].value,
        'end': elem.target[1].value,
        'replacement': elem.target[2].value
    }
    app.callTransform();
    // console.log(json)
}
function calculate(elem){
    var recodeString = elem.target[0].value;
    var json = {
        'configuration': config,
        'variable': currentVal,
        'formula': recodeString
    }

    console.log(json);

}

function createClick(){
    var elem = document.getElementById('formulaDiv');
    elem.style.display="none";

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";

    var elem = document.getElementById('nominalDiv');
    elem.style.visibility="hidden";
    
    var elem = document.getElementById('submitButton');
    elem.style.visibility="hidden";

    var elem = document.getElementById('recodeDiv');
    elem.style.display = 'none';

    var elem = document.getElementById('createLink');
    elem.className = 'active';
    var elem = document.getElementById('formulaLink');
    elem.className = '';
    var elem = document.getElementById('recodeLink');
    elem.className = '';
    var elem = document.getElementById('reorderLink');
    elem.className = '';

    var elem = document.getElementById('leftpanel');
    elem.style.display ='none';
    
    var elem = document.getElementById('rightpanel');
    elem.style.display ='none';
    
    var elem = document.getElementById('centralPanel');
    elem.style.display ='block';
    
}
function createCreate(){

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";
    var elem = document.getElementById('formulaDiv');
    elem.style.display = 'none';
    var elem = document.getElementById('centralPanel');
    elem.style.display ='none';
}

function recodeCreate(){
    var elem = document.getElementById('recodeLink');
    elem.className = 'active';

    var elem = document.getElementById('centralPanel');
    elem.style.display ='none';
}
function recodeClick(){
    var elem = document.getElementById('recodeLink');
    elem.className = 'active';

    var elem = document.getElementById('centralPanel');
    elem.style.display="none"

    var elem = document.getElementById('rightpanel');
    elem.style.display="block"

    var elem = document.getElementById('leftpanel');
    elem.style.display="block"

    var elem = document.getElementById('recodeDiv');
    elem.style.display="block";

    var elem = document.getElementById('formulaDiv');
    elem.style.display="none";

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";

    var elem = document.getElementById('reorderLink');
    elem.className = '';
    var elem = document.getElementById('formulaLink');
    elem.className = '';
    var elem = document.getElementById('createLink');
    elem.className = '';
}

function formulaCreate(){

    var elem = document.getElementById('centralPanel');
    elem.style.display ='none';
}
function formulaClick(){

    var elem = document.getElementById('centralPanel');
    elem.style.display ='none';

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";
    
    var elem = document.getElementById('rightpanel');
    elem.style.display="block"

    var elem = document.getElementById('leftpanel');
    elem.style.display="block"

    var elem = document.getElementById('formulaDiv');
    elem.style.display="block";

    var elem = document.getElementById('recodeDiv');
    elem.style.display = 'none';  
    
    var elem = document.getElementById('formulaLink');
    elem.className = 'active';
    var elem = document.getElementById('reorderLink');
    elem.className = '';
    var elem = document.getElementById('recodeLink');
    elem.className = '';
    var elem = document.getElementById('createLink');
    elem.className = '';
}

function reorderClick(){
    var elem = document.getElementById('reorderLink');
    elem.className = 'active';
    var elem = document.getElementById('formulaLink');
    elem.className = '';
    var elem = document.getElementById('recodeLink');
    elem.className = '';
    var elem = document.getElementById('createLink');
    elem.className = '';

    var elem = document.getElementById('formulaDiv');
    elem.style.display="none";

    var elem = document.getElementById('recodeDiv');
    elem.style.display = 'none';

    var elem = document.getElementById('centralPanel');
    elem.style.display ='none'; 

    var elem = document.getElementById('leftpanel');
    elem.style.display ='block';
    
    var elem = document.getElementById('rightpanel');
    elem.style.display ='block';

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";
    
}
function reorderCreate(){

    var elem = document.getElementById('centralPanel');
    elem.style.display ='none';

    var elem = document.getElementById('createTableDiv');
    elem.style.display="none";
}

$(window).resize(function(){
    $('#rightpanel').css({
        'width':  (window.innerWidth - 300)+'px',
    });
    $('#centralPanel').css({
        'width':  (window.innerWidth-25)+'px',
    });
    $('#tableDiv').css({
        'height': (window.innerHeight-150)+'px',
    });
});