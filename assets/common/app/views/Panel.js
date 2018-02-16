import m from 'mithril';
import '../../css/common.css'

import {
    panelOpen,
    togglePanelOpen,
    menuColor,
    borderColor,
    heightHeader,
    heightFooter,
    panelMargin,
    canvasScroll,
    scrollbarWidth,
    setPanelOcclusion
} from "../common";

// ```
// m(Panel, {
//     side: 'left' || 'right',
//     label: 'text at top of header',
//     hover: Bool
//     contents: m(...),
//     width: int pixels
//     })
// ```

// If hover is true, then the canvas is occluded by the panels.
// If hover is false, then the canvas is resized to maintain a margin as panels are opened/closed or canvas contents overflow.
//
// Contents for each partition are described in the sections list.
// If tabs are set, then the width of the panel may be set in the section options.
// If headers are set, the width of the panel is set globally.
//
// Sometimes the contents of a panel should not be accessible via the tab bar.
// Setting the toggle 'visible' prevents it from appearing in the tab bar.

export default class Panel {

    view(vnode) {
        let {side, hover, label, contents, width} = vnode.attrs;
        const dot = [m.trust('&#9679;'), m('br')];

        if (!hover) {
            setPanelOcclusion(side, (panelOpen[side] ? width : 16) + 2 * panelMargin);
        }

        return m(`#${side}panel.container.sidepanel.clearfix`, {
            style: {
                background: menuColor,
                border: borderColor,
                width: panelOpen[side] ? width + 'px' : 0,
                height: `calc(100% - ${heightHeader + heightFooter}px - ${2 * panelMargin}px - ${canvasScroll['horizontal'] ? scrollbarWidth : 0}px)`,
                position: 'fixed',
                top: heightHeader + panelMargin + 'px',
                [side]: (side === 'right' && canvasScroll['vertical'] ? scrollbarWidth : 0) + panelMargin + 'px',
                ['padding-' + side]: '1px',
                'z-index': 100
            }
        }, [
            // Panel handle
            m(`#toggle${side === 'left' ? 'L' : 'R'}panelicon.panelbar`, {
                    style: {height: '100%', [side]: 'calc(100% - 16px)'}
                },
                m('span', {onclick: () => togglePanelOpen(side)}, dot, dot, dot, dot)),

            // Panel contents
            m(`div${panelOpen[side] ? '' : '.closepanel'}`, {
                    style: {
                        width: 'calc(100% - 8px)', height: '100%',
                        margin: '0 4px',
                        display: panelOpen[side] ? 'block' : 'none'
                    }
                },
                [
                    m(`#${side}paneltitle.panel-heading.text-center`, m("h3.panel-title", label)),
                    contents
                ])
        ])
    }
}