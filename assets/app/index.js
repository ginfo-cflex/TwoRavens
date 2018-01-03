import '../css/app.css';
import '../pkgs/bootstrap/css/bootstrap-theme.min.css';
import '../pkgs/Ladda/dist/ladda-themeless.min.css';
import '../../node_modules/hopscotch/dist/css/hopscotch.css';

import m from 'mithril';

import * as app from './app';
import * as exp from './explore';
import * as plots from './plots';
import Panel from './views/Panel';
import Button, {when} from './views/PanelButton';
import List from './views/PanelList';
import Search from './views/Search';
import Subpanel from './views/Subpanel';

function setBackgroundColor(color) {
    return function() {
        this.style['background-color'] = color;
    };
}

function leftpanel() {
    return m(
        Panel,
        {side: 'left',
         title: 'Data Selection',
         buttons: [
             m(Button,
               {id: 'btnVariables',
                id2: 'tab1',
                title: 'Click variable name to add or remove the variable pebble from the modeling space.'},
               'Variables'),
             m(Button, {id: 'btnSubset', id2: 'tab2'}, 'Subset'),
             m(Button,
               {id: 'btnSelect',
                classes: 'btn-default.ladda-button[data-spinner-color=#000000][data-style=zoom-in]',
                onclick: _ => app.subsetSelect('btnSelect'),
                style: `display: ${app.subset ? 'block' : 'none'}; float: right; margin-right: 10px`,
                title: 'Subset data by the intersection of all selected values.'},
               m('span.ladda-label[style=pointer-events: none]', 'Select'))]},
        m(`#tab1[style=display: ${when('left', 'tab1')}; padding: 0 8px; text-align: center]`,
          m(Search, {placeholder: 'Search variables and labels'}),
          m(List, {items: app.valueKey, title: 'Summary Statistics'})),
        m(`#tab2[style=display: ${when('left', 'tab2')}; margin-top: .5em]`),
        m('#tab3[style=height: 350px]',
          m(`p[style=padding: .5em 1em; display: ${when('left', 'tab3')}]`,
            {title: "Select a variable from within the visualization in the center panel to view its summary statistics."},
            m('center',
              m('b', app.summary.name),
              m('br'),
              m('i', app.summary.labl)),
            m('table', app.summary.data.map(
                tr => m('tr', tr.map(
                    td => m('td', {onmouseover: setBackgroundColor('aliceblue'), onmouseout: setBackgroundColor('f9f9f9')},
                            td))))))));
}

let righttab = (id, btnId, task, title, probDesc) => m(
    `#${id}[style=display: ${when('right', btnId)}; padding: 6px 12px; text-align: center]`,
    m(List,
      {items: Object.keys(task || {}),
       title: title + ' Description',
       content: v => task[v][1],
       probDesc: probDesc}));

function rightpanel(mode) {
    return mode ?
        m(Panel,
          {side: 'right',
           title: 'Result Exploration',
           buttons: [
               m(Button, {id: 'btnUnivariate'}, 'Univariate'),
               m(Button, {id: 'btnBivariate'}, 'Bivariate')]},
          m(`#univariate[style=display: ${when('right', 'btnUnivariate')}]`),
          m(`#bivariate[style=display: ${when('right', 'btnBivariate')}]`),
          m('#result_left[style=width: 50%; height: 90%; float:left;background-color: white; display:none; border-right:groove; border-bottom: groove; overflow:auto; white-space:nowrap]',
            m('#scatterplot.container3[style=width: 500px; height: 50%; background-color: white; display: none; overflow: auto]'),
            m('#heatchart.container4[style=width: 500px; height:60%; display: none; overflow: auto; background-color: #FFEBEE'),
            m('h5#NAcount[style=margin-left: 20px; display: none]'),
            m('button#linechart_welcome.btn.btn-danger[type=button][onclick=linechart()][style=display: block; margin: 0 auto; position: relative]', 'Linechart'),
            m('#linechart.container4[width: 500px; height: 320px; background-color: white; display: none; overflow:auto')),
          m('#result_right[style=width: 50%; height: 90%; float: right; background-color: white; display:none; border-right: groove; border-bottom: groove; overflow: auto; white-space: nowrap]',
            m('.btn-group.btn-toggle.col-md-4.text-center[style=width:auto; height:10%; display:block; white-space:nowrap; margin-left:170px; position:relative]',
              m('button#selection.btn.btn-primary.active', 'Selection'),
              m('button#crossTabs.btn.btn-default', 'Cross-Tabs')),
            m('#resultsView_tabular.container1[style=width:100%; height:60%; background-color:white; display:block; float:left; overflow:auto; white-space:nowrap; border-right:groove; border-bottom:groove]'),
            m('#resultsView_statistics.container2[style=width:100%; height:40%; background-color:white; display:none; clear:left; float:right; overflow:auto; white-space:nowrap; border-right:groove; border-bottom:groove')),
          m('#resultsHolder')) :
    // mode == null (model mode)
    m(Panel,
      {side: 'right',
       title: 'Model Selection',
       buttons: [
           m(Button, {id: 'btnModels', style: 'width: 100%'}, 'Models'),
           m(Button, {id: 'btnSetx', style: 'width: 100%'}, 'Set Covar.'),
           m(Button, {id: 'btnResults', style: 'width: 100%'}, 'Results'),
           m(Button, {id: 'btnType', style: 'width: 100%'}, 'Task Type'),
           m(Button, {id: 'btnSubtype', style: 'width: 100%'}, 'Subtype'),
           m(Button, {id: 'btnMetrics', style: 'width: 100%'}, 'Metrics')]},
     //      m(Button, {id: 'btnOutputs', style: 'width: 100%'}, 'Output')]},
      m(`#results[style=display: ${when('right', 'btnResults')}; margin-top: .5em]`,
        m("#resultsView.container[style=float: right; overflow: auto; width: 80%; background-color: white; white-space: nowrap]"),
        m('#modelView[style=display: none; float: left; width: 20%; background-color: white]'),
        m("p#resultsHolder[style=padding: .5em 1em]")),
      m(`#setx[style=display: ${when('right', 'btnSetx')}]`,
        m('#setxLeftAll[style=display:block; float: left; width: 30%; height:100%; background-color: white]',
          m('#setxLeft[style=display:block; float: left; width: 100%; height:95%; overflow:auto; background-color: white]')),
        m('#setxRightAll[style=display:block; float: left; width: 70%; height:100%; background-color: white]',
          m('#setxRightTop[style=display:block; float: left; width: 100%; height:65%; overflow:auto; background-color: white]',
            m('#setxMiddle[style=display:block; float: left; width: 70%; height:100%; background-color: white]'),
            m('#setxRight[style=display:block; float: right; width: 30%; height:100%; background-color: white]'))),
        m('#setxRightBottom[style=display:block; float: left; width: 100%; height:35%; overflow:auto; background-color: white]',
          m('#setxRightBottomLeft[style=display:block; float: left; width: 75%; height:100%; background-color: white]'),
          m('#setxRightBottomMiddle[style=display:block; float: left; width: 15%; height:100%; background-color: white]',
            m(Button,
              {id: 'btnExecutePipe',
               classes: 'btn-default.ladda-button[data-spinner-color=#000000][data-style=zoom-in]',
               onclick: _ => app.executepipeline('btnExecutePipe'),
               style: `display:inline; float: left; margin-right: 10px`,
               title: 'Execute pipeline.'},
              m('span.ladda-label[style=pointer-events: none]', 'Execute'))),
          m('#setxRightBottomRight[style=display:block; float: left; width: 10%; height:100%; background-color: white]'))),
      righttab('models', 'btnModels'),
      righttab('types', 'btnType', app.d3mTaskType, 'Task', 'taskType'),
      righttab('subtypes', 'btnSubtype', app.d3mTaskSubtype, 'Task Subtype', 'taskSubtype'),
      righttab('metrics', 'btnMetrics', app.d3mMetrics, 'Metric', 'metric'));
//      righttab('outputs', 'btnOutputs', app.d3mOutputType, 'Output', 'outputType'));
}

let ticker = mode => {
    let link = name => m(`a${name === mode ? '.active' : ''}[href=/${name}][style=margin-right: 0.5em]`, {oncreate: m.route.link}, name[0].toUpperCase() + name.slice(1));
    return m('#ticker[style=background: #F9F9F9; bottom: 0; height: 40px; position: fixed; width: 100%; border-top: 1px solid #ADADAD]',
        link('model'),
        link('explore'),
        m("a#logID[href=somelink][target=_blank][style=margin-right: 0.5em]", "Replication"),
        // dev links...
        m("a[href='/dev-raven-links'][target=_blank][style=margin-right: 0.5em]", "raven-links"),
        m("a[style=margin-right: 0.5em]",
          {onclick: app.record_user_metadata},
          "record-metadata"));
};

class Body {
    oninit() {
        this.about = false;
        this.cite = false;
        this.citeHidden = false;
    }

    oncreate() {
        let extract = (name, key, offset, replace) => {
            key = key + '=';
            let loc = window.location.toString();
            let val = loc.indexOf(key) > 0 ? loc.substring(loc.indexOf(key) + offset) : '';
            let idx = val.indexOf('&');
            val = idx > 0 ? val.substring(0, idx) : val;
            val = val.replace('#!/model', '');
            console.log(name, ': ', val);
            if (replace) val = val
                .replace(/%25/g, '%')
                .replace(/%3A/g, ':')
                .replace(/%2F/g, '/');
            return val;
        };
        app.main(
            extract('fileid', 'dfId', 5),
            extract('hostname', 'host', 5),
            extract('ddiurl', 'ddiurl', 7, true),
            extract('dataurl', 'dataurl', 8, true),
            extract('apikey', 'key', 4));
    }

    view(vnode) {
        let {mode} = vnode.attrs;
        let explore = mode === 'explore';
        app.is_results_mode = mode === 'results';

        let _navBtn = (id, left, right, onclick, args, min) => m(
            `button#${id}.btn.navbar-right`,
            {onclick: onclick,
             style: {'margin-left': left + 'em',
                     'margin-right': right + 'em',
                     'min-width': min}},
            args);
        let navBtn = (id, left, right, onclick, args, min) => _navBtn(
            id + '.ladda-button[data-spinner-color=#000000][data-style=zoom-in]',
            left, right, onclick, args, min);
        let navBtn1 = (id, onclick, args, title) => _navBtn(
            `${id}.btn-default[title=${title}]`, 2, 0, onclick, args);
        let glyph = (icon, unstyled) => m(
            `span.glyphicon.glyphicon-${icon}` + (unstyled ? '' : '[style=color: #818181; font-size: 1em; pointer-events: none]'));
        let transformation = (id, list) => m(
            `ul#${id}`, {
                style: {display: 'none', 'background-color': app.varColor},
                onclick: function(evt) {
                    // if interact is selected, show variable list again
                    if ($(this).text() === 'interact(d,e)') {
                        $('#tInput').val(tvar.concat('*'));
                        selInteract = true;
                        $(this).parent().fadeOut(100);
                        $('#transSel').fadeIn(100);
                        evt.stopPropagation();
                        return;
                    }

                    let tvar = $('#tInput').val();
                    let tfunc = $(this).text().replace("d", "_transvar0");
                    let tcall = $(this).text().replace("d", tvar);
                    $('#tInput').val(tcall);
                    $(this).parent().fadeOut(100);
                    evt.stopPropagation();
                    transform(tvar, tfunc, typeTransform = false);
                }
            },
            list.map(x => m('li', x)));
        let spaceBtn = (id, onclick, title, icon) => m(
            `button#${id}.btn.btn-default`, {onclick, title}, glyph(icon, true));

        return m(
            'main',
            m("nav#navbar.navbar.navbar-default.navbar-fixed-top[role=navigation]",
              {style: mode === 'explore' && 'background-image: -webkit-linear-gradient(top, #fff 0, rgb(227, 242, 254) 100%)'},
              m("a.navbar-brand",
                m("img[src=/static/images/TwoRavens.png][alt=TwoRavens][width=100][style=margin-left: 1em; margin-top: -0.5em]",
                  {onmouseover: _ => this.about = true, onmouseout: _ => this.about = false})),
              m('#navbarNav[style=padding: 0.5em]',
                m('#dataField.field[style=margin-top: 0.5em; text-align: center]',
                  m('h4#dataName[style=display: inline]',
                    {onclick: _ => this.cite = this.citeHidden = !this.citeHidden,
                     onmouseout: _ => this.citeHidden || (this.cite = false),
                     onmouseover: _ => this.cite = true},
                    "Dataset Name"),
                  m('#cite.panel.panel-default',
                    {style: `display: ${this.cite ? 'block' : 'none'}; position: absolute; right: 50%; width: 380px; text-align: left; z-index: 50`},
                    m(".panel-body")),
                  m('span',
                    navBtn('btnEstimate.btn-success', 2, 1, explore ? exp.explore : app.estimate, m("span.ladda-label", explore ? 'Explore' : 'Solve This Problem'), '150px'),
                    navBtn('btnTA2.btn-default', .5, 1, _ => app.helpmaterials('manual'), ['Help Manual ', glyph('book')]),
                    navBtn('btnTA2.btn-default', 2, .5, _ => app.helpmaterials('video'), ['Help Video ', glyph('expand')]),
                    navBtn1("btnReset", app.reset, glyph('repeat'), 'Reset'),
                    navBtn1('btnEndSession', app.endsession, m("span.ladda-label", 'Mark Problem Finished'), 'Mark Problem Finished')),
                  m('#tInput', {
                      style: {display: 'none'},
                      onclick: _ => {
                          if (byId('transSel').style.display !== 'none') { // if variable list is displayed when input is clicked...
                              $('#transSel').fadeOut(100);
                              return false;
                          }
                          if (byId('transList').style.display !== 'none') { // if function list is displayed when input is clicked...
                              $('#transList').fadeOut(100);
                              return false;
                          }

                          // highlight the text
                          $(this).select();
                          let pos = $('#tInput').offset();
                          pos.top += $('#tInput').width();
                          $('#transSel').fadeIn(100);
                          return false;
                      },
                      keyup: evt => {
                          let t = byId('transSel').style.display;
                          let t1 = byId('transList').style.display;
                          if (t !== 'none') {
                              $('#transSel').fadeOut(100);
                          } else if (t1 !== 'none') {
                              $('#transList').fadeOut(100);
                          }

                          if (evt.keyCode == 13) { // keyup on Enter
                              let t = transParse($('#tInput').val());
                              if (!t) {
                                  return;
                              }
                              transform(t.slice(0, t.length - 1), t[t.length - 1], typeTransform = false);
                          }
                      }
                  }),
                  m('#transformations.transformTool', {
                      title: `Construct transformations of existing variables using valid R syntax.
                              For example, assuming a variable named d, you can enter "log(d)" or "d^2".`},
                    transformation('transSel', ['a', 'b']),
                    transformation('transList', app.transformList)))),
              m(`#about.panel.panel-default[style=display: ${this.about ? 'block' : 'none'}; left: 140px; position: absolute; width: 500px; z-index: 50]`,
                m('.panel-body',
                  'TwoRavens v0.1 "Dallas" -- The Norse god Odin had two talking ravens as advisors, who would fly out into the world and report back all they observed. In the Norse, their names were "Thought" and "Memory". In our coming release, our thought-raven automatically advises on statistical model selection, while our memory-raven accumulates previous statistical models from Dataverse, to provide cummulative guidance and meta-analysis.'))),
            m(`#main.left.carousel.slide.svg-leftpanel.svg-rightpanel[style=overflow: hidden]`,
              m("#innercarousel.carousel-inner",
                m('#m0.item.active',
                  m('svg#whitespace'))),
              m("#spacetools.spaceTool[style=z-index: 16]",
                spaceBtn('btnLock.active', app.lockDescription, 'Lock selection of problem description', 'pencil'),
                spaceBtn('btnJoin', _ => {
                    let links = [];
                    if (explore) {
                        let is_unique = (n, n1) => links.map(l => l.target === n1 && l.source === n).length == 0;
                        links = app.nodes.map(n => app.nodes.filter(n1 => n !== n1 && is_unique(n, n1)).map(n1 => ({
                            left: false,
                            right: false,
                            target: n,
                            source: n1,
                        })));
                    } else {
                        let dvs = app.nodes.filter(n => app.zparams.zdv.includes(n.name));
                        let ivs = app.nodes.filter(n => !dvs.includes(n));
                        links = dvs.map(dv => ivs.map(iv => ({
                            left: true,
                            right: false,
                            target: iv,
                            source: dv,
                        })));
                    }
                    app.restart([].concat(...links));
                }, 'Make all possible connections between nodes', 'link'),
                spaceBtn('btnDisconnect', _ => app.restart([]), 'Delete all connections between nodes', 'remove-circle'),
                spaceBtn('btnForce', app.forceSwitch, 'Pin the variable pebbles to the page', 'pushpin'),
                spaceBtn('btnEraser', app.erase, 'Wipe all variables from the modeling space', 'magnet')),
              m(Subpanel,
                {title: "Legend",
                 buttons: [
                     ['timeButton', 'ztime', 'Time'],
                     ['csButton', 'zcross', 'Cross Sec'],
                     ['dvButton', 'zdv', 'Dep Var'],
                     ['nomButton', 'znom', 'Nom Var'],
                     ['gr1Button', 'zgroup1', 'Group 1'],
                     ['gr2Button', 'zgroup2', 'Group 2']]}),
              m(Subpanel, {title: "History"}),
              ticker(mode),
              leftpanel(),
              rightpanel(mode)));
    }
}

m.route(document.body, '/model', {
    '/model': {render: () => m(Body)},
    '/explore': {render: () => m(Body, {mode: 'explore'})},
    '/results': {render: () => m(Body, {mode: 'results'})},
});
