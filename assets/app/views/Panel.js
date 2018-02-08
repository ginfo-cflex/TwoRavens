import m from 'mithril';

import * as app from '../app';

export let getClasses = function(cls, panel, attrs) {
    if (panel.closed) {
        cls += '.closepanel';
    } else if (attrs.is_explore_mode) {
        if (app.righttab === 'btnUnivariate' && attrs.univariate_finished) {
            cls += `[style=width: ${45}%]`;
        } else if (app.righttab === 'btnBivariate') {
            cls += `[style=width: ${75}%]`;
        }
    } else if (app.lefttab === 'tab2') {
        cls += '.expandpanel';
    }
    return cls;
;
};

class Panel {
    oninit(vnode) {
        this.closed = false;
    }

    view(vnode) {
        let {side, title, buttons, is_explore_mode} = vnode.attrs;
        let btns = buttons;
        let dot = [m.trust('&#9679;'), m('br')];
        let width = 100 / btns.length;
        let expandwidth = 35;
        let shrinkwidth = 65 / (btns.length - 1);
        return m(
            getClasses(`#${side}panel.sidepanel.container.clearfix`, this, vnode.attrs),
            m(`#toggle${side === 'left' ? 'L' : 'R'}panelicon.panelbar[style=height: 100%]`,
              m('span', {onclick: _ => this.closed = !this.closed}, dot, dot, dot, dot)),
            m(`#${side}paneltitle.panel-heading.text-center`,
              m("h3.panel-title", title)),
            m(`ul${side === 'right' ? '#rightpanelbuttons' : ''}.accordion`,
              btns.map(b => {
                  b.attrs.style = b.attrs.style + '; width: 100%';
                  b.attrs.is_explore_mode = is_explore_mode;
                  let id = b.attrs.id;
                  //let w = this.active_btn === id ? shrinkwidth :
                      //this.active_btn === null ? width :
                      //expandwidth;
                  return m(
                      'li',
                      {style: {width: width + '%', 'max-width': '150px'},
                       onmouseover: _ => this.active_btn = id,
                       onmouseout: _ => this.active_btn = null},
                      b);
              })),
            m('.row-fluid',
              m(`#${side}panelcontent`,
                m(`#${side}ContentArea[style=height: calc(100vh - 213px); overflow: auto]`, vnode.children))));
    }
}

export default Panel;
