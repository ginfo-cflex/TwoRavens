import m from 'mithril';

import * as app from './app'
import * as aggreg from './aggreg/aggreg'
import * as tour from "./tour";

import {tableHeight} from "./aggreg/aggreg";

import * as common from '../../common/app/common'
import {panelMargin, heightHeader, heightFooter, canvasScroll, scrollbarWidth} from "../../common/app/common";

import Panel from '../../common/app/views/Panel'
import Header from '../../common/app/views/Header'
import Footer from '../../common/app/views/Footer'
import Canvas from '../../common/app/views/Canvas'
import MenuTabbed from '../../common/app/views/MenuTabbed'
import MenuHeaders from '../../common/app/views/MenuHeaders'
import PanelList from '../../common/app/views/PanelList'
import TextField from '../../common/app/views/TextField'

import CanvasAction from "./views/CanvasAction"
import CanvasActor from "./views/CanvasActor"
import CanvasCoordinates from "./views/CanvasCoordinates"
import CanvasCustom from "./views/CanvasCustom"
import CanvasDate from "./views/CanvasDate"
import CanvasLocation from "./views/CanvasLocation"
import CanvasPentaClass from "./views/CanvasPentaClass"
import CanvasRootCode from "./views/CanvasRootCode"

import TableAggregation from "./views/TableAggregation"

export default class Body_EventData {

    oninit(vnode) {
        if (vnode.attrs.mode !== 'subset') {
            m.route.set('/subset');
            vnode.attrs.mode = 'subset';
        }
    }

    oncreate() {
        app.setupBody();
        app.setupQueryTree();
        aggreg.setupAggregation();
    }

    header(mode) {
        return m(Header, {
            contents: [

                // Button Reset
                m("button.btn.btn-default.ladda-button[data-spinner-color='#818181'][data-style='zoom-in'][id='btnReset'][title='Reset']", {
                        style: {
                            "margin-left": "2.0em",
                            "float": "right"
                        },
                        onclick: app.reset
                    },
                    m("span.ladda-label.glyphicon.glyphicon-repeat", {
                        style: {
                            "font-size": "1em",
                            "color": "#818181",
                            "pointer-events": "none"
                        }
                    })
                ),

                // Button Subset Submit
                m("label#btnSubsetSubmit.btn.btn-default.ladda-button[data-spinner-color='#818181'][data-style='zoom-in']", {
                        style: {
                            "float": "right",
                            "margin-left": "2em",
                            "margin-right": "1em"
                        },
                        onclick: () => (mode==='subset') ? app.submitQuery() : m.route.set('/subset')
                    },
                    m("span.ladda-label", "Subset")
                ),

                // Button Aggregate
                m("button.btn.btn-default[id='aggSubmit']",
                    {
                        style: {"margin-right": "1em", 'float': 'right'},
                        onclick: () => (mode==='subset') ? m.route.set('/aggregate') : undefined
                    }, "Aggregate"),

                // Dataset Selection
                m("div", {
                        style: {"left": "calc(50% + 20px)", "position": "fixed"},
                        onclick: function (e) {
                            // I could not get these to bind, so I bind them on dropdown
                            $("#selectPhoenixRT").click(function () {
                                app.setDataset('phoenix_rt');
                            });
                            $("#selectClineNYT").click(function () {
                                app.setDataset('cline_phoenix_nyt');
                            });
                            $("#selectClineCIA").click(function () {
                                app.setDataset('cline_phoenix_fbis');
                            });
                            $("#selectClineSWB").click(function () {
                                app.setDataset('cline_phoenix_swb');
                            });
                            $("#selectICEWS").click(function () {
                                app.setDataset('icews');
                            });
                        }
                    },
                    m(".popover-markup", {style: {"display": "inline"}},
                        [
                            m("a.trigger.btn.btn-sm.btn-default", {style: {"height": "30px"}},
                                m("span.glyphicon.glyphicon-chevron-down", {
                                    style: {
                                        "margin-top": "3px",
                                        "font-size": "1em",
                                        "color": "#818181",
                                        "pointer-events": "none"
                                    }
                                })
                            ),
                            m(".head.hide",
                                "Dataset"
                            ),
                            m(".content.hide",
                                m(".popoverContentContainer",
                                    [
                                        m("[id='optionMenu']",
                                            [
                                                m("button.btn.btn-default[data-option='1'][id='option']", "Phoenix - UTDallas"),
                                                m("button.btn.btn-default[data-option='2'][id='option']", "Cline - New York Times"),
                                                m("button.btn.btn-default[data-option='3'][id='option']", "Cline - CIA Broadcast"),
                                                m("button.btn.btn-default[data-option='4'][id='option']", "Cline - BBC Summary"),
                                                m("button.btn.btn-default[data-option='5'][id='option']", "ICEWS")
                                            ]
                                        ),

                                        m(".optionView[id='optionView1']",
                                            [
                                                m("button.btn.btn-sm.btn-default[data-option='1'][id='option']",
                                                    m("span.glyphicon.glyphicon-chevron-left", {
                                                        style: {
                                                            "font-size": "1em",
                                                            "color": "#818181",
                                                            "pointer-events": "none"
                                                        }
                                                    })
                                                ),
                                                m("p", "A Phoenix-coded event dataset constructed here at The University of Texas at Dallas!"),
                                                m("button.btn.btn-primary[id='selectPhoenixRT']", m("span.ladda-label", "Select"))
                                            ]
                                        ),

                                        m(".optionView[id='optionView2']",
                                            [
                                                m("button.btn.btn-sm.btn-default[data-option='2'][id='option']",
                                                    m("span.glyphicon.glyphicon-chevron-left", {
                                                        style: {
                                                            "font-size": "1em",
                                                            "color": "#818181",
                                                            "pointer-events": "none"
                                                        }
                                                    })
                                                ),
                                                m(".head", {style: {"margin-left": "40px"}},
                                                    m("a[href='http://www.clinecenter.illinois.edu/data/event/phoenix/']", "Cline New York Times")
                                                ),
                                                m("p", "This data is sourced from the New York Times and collected by the Cline Center for Advanced Social Research."),
                                                m("button.btn.btn-primary[id='selectClineNYT']", m("span.ladda-label", "Select"))
                                            ]
                                        ),

                                        m(".optionView[id='optionView3']",
                                            [
                                                m("button.btn.btn-sm.btn-default[data-option='3'][id='option']",
                                                    m("span.glyphicon.glyphicon-chevron-left", {
                                                        style: {
                                                            "font-size": "1em",
                                                            "color": "#818181",
                                                            "pointer-events": "none"
                                                        }
                                                    })
                                                ),
                                                m(".head", {style: {"margin-left": "40px"}},
                                                    m("a[href='http://www.clinecenter.illinois.edu/data/event/phoenix/']", "Cline CIA Broadcast")
                                                ),
                                                m("p", "This data is sourced from the CIA Foreign Broadcast Information Service and collected by the Cline Center for Advanced Social Research."),
                                                m("button.btn.btn-primary[id='selectClineCIA']", m("span.ladda-label", "Select"))
                                            ]
                                        ),

                                        m(".optionView[id='optionView4']",
                                            [
                                                m("button.btn.btn-sm.btn-default[data-option='4'][id='option']",
                                                    m("span.glyphicon.glyphicon-chevron-left", {
                                                        style: {
                                                            "font-size": "1em",
                                                            "color": "#818181",
                                                            "pointer-events": "none"
                                                        }
                                                    })
                                                ),
                                                m(".head", {style: {"margin-left": "40px"}},
                                                    m("a[href='http://www.clinecenter.illinois.edu/data/event/phoenix/']", "Cline BBC Summary")
                                                ),
                                                m("p", "This data is sourced from the BBC Summary of World Broadcasts and collected by the Cline Center for Advanced Social Research."),
                                                m("button.btn.btn-primary[id='selectClineSWB']", m("span.ladda-label", "Select"))
                                            ]
                                        ),

                                        m(".optionView[id='optionView5']",
                                            [
                                                m("button.btn.btn-sm.btn-default[data-option='5'][id='option']",
                                                    m("span.glyphicon.glyphicon-chevron-left", {
                                                        style: {
                                                            "font-size": "1em",
                                                            "color": "#818181",
                                                            "pointer-events": "none"
                                                        }
                                                    })
                                                ),
                                                m(".head", {style: {"margin-left": "40px"}},
                                                    m("a[href='https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075']", "ICEWS Coded Event Data")
                                                ),
                                                m("p", "Event data consists of coded interactions between socio-political actors (i.e., cooperative or hostile actions between individuals, groups, sectors and nation states)."),
                                                m("button.btn.btn-primary[id='selectICEWS']", m("span.ladda-label", "Select"))
                                            ]
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                ),
                m("h4", {style: {"right": "calc(50% - 10px)", "position": "fixed"}},
                    m("span.label.label-default[id='datasetLabel']")
                )
            ]
        })
    }

    footer(mode) {
        return m(Footer, {
            contents: [
                m("span.label.label-default", {style: {"margin-left": "10px", "display": "inline-block"}}, "Tours"),
                (mode === 'subset') ?
                    m("div[id='subsetTourBar']", {style: {"display": "inline-block"}},
                        [
                            m("button.btn.btn-default.btn-sm[id='tourButtonGeneral'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartGeneral
                            }, "General"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonActor'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartActor
                            }, "Actor"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonDate'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartDate
                            }, "Date"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonAction'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartAction
                            }, "Action"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonLocation'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartLocation
                            }, "Location"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonCoordinates'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartCoordinates
                            }, "Coordinates"),
                            m("button.btn.btn-default.btn-sm[id='tourButtonCustom'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartCustom
                            }, "Custom")
                        ]) :
                    m("div[id='aggregTourBar']", {style: {"display": "inline-block"}},
                        [
                            m("button.btn.btn-default.btn-sm[id='tourButtonAggreg'][type='button']", {
                                style: {
                                    "margin-left": "5px",
                                    "margin-top": "4px"
                                },
                                onclick: tour.tourStartAggregation
                            }, "Aggregation")
                        ]),

                // Record Count
                m("span.label.label-default[id='recordCount']", {
                    style: {
                        "display": "inline-block",
                        "margin-top": "10px",
                        "margin-right": "10px",
                        "float": "right"
                    }
                })
            ]
        })
    }

    leftpanel(mode) {
        if (mode === 'subset') {
            return m(Panel, {
                side: 'left',
                label: 'Data Selection',
                hover: false,
                width: '250px',
                contents: m(MenuTabbed, {
                    id: 'leftPanelMenu',
                    callback: app.setLeftTab,
                    currentTab: app.leftTab,
                    attrsAll: {style: {height: 'calc(100% - 39px)'}},
                    sections: [
                        {
                            value: 'Variables',
                            title: 'Restrict by data column.',
                            contents: [
                                m(TextField, {
                                    id: 'searchVariables',
                                    placeholder: 'Search variables',
                                    oninput: app.reloadLeftpanelVariables
                                }),
                                m(PanelList, {
                                    id: 'variablesList',
                                    items: app.matchedVariables,
                                    colors: {[common.selVarColor]: app.variablesSelected},
                                    callback: app.toggleVariableSelected,
                                    attrsAll: {style: {height: 'calc(100% - 44px)', overflow: 'auto'}}
                                })
                            ]
                        },
                        {
                            value: 'Subsets',
                            title: 'Restrict by contents of rows.',
                            contents: m(PanelList, {
                                id: 'subsetsList',
                                items: app.subsetKeys,
                                colors: {[common.selVarColor]: [app.canvasKeySelected]},
                                callback: app.showCanvas,
                                attrsAll: {style: {height: 'calc(100% - 39px)', overflow: 'auto'}}
                            })
                        }
                    ]
                })
            })
        }

        if (mode === 'aggregate') {
            return m(Panel, {
                id: 'leftPanelMenu',
                side: 'left',
                width: '250px',
                label: 'Data Selection',
                attrsAll: {style: {
                    height: `calc(100% - ${heightHeader + heightFooter}px - ${2 * panelMargin}px - ${canvasScroll['horizontal'] ? scrollbarWidth : 0}px - ${tableHeight})`
                }},
                contents: m(MenuHeaders, {
                    id: 'aggregateMenu',
                    sections: [
                        {
                            value: 'Unit of Measure',
                            contents: m(PanelList, {
                                items: ['Date', 'Actor'],
                                colors: {[common.selVarColor]: [app.canvasKeySelected]},
                                callback: app.showCanvas
                            })
                        },
                        {
                            value: 'Event Measure',
                            contents: m(PanelList, {
                                items: ['Penta Class', 'Root Code'],
                                colors: {[common.selVarColor]: [app.canvasKeySelected]},
                                callback: app.showCanvas
                            })
                        }
                    ]
                })
            })
        }
    }

    rightpanel(mode) {
        return m(Panel, {
            id: 'rightPanelMenu',
            side: 'right',
            label: 'Query Summary',
            width: '250px',
            attrsAll: {style: mode === 'aggregate' ? {
                    height: `calc(100% - ${heightHeader + heightFooter}px - ${2 * panelMargin}px - ${canvasScroll['horizontal'] ? scrollbarWidth : 0}px - ${tableHeight})`
                } : {}},
            contents: [
                m(MenuHeaders, {
                    id: 'querySummaryMenu',
                    attrsAll: {style: {height: 'calc(100% - 85px)', overflow: 'auto'}},
                    sections: [
                        {value: 'Variables', contents: m('div#variableTree')},
                        {value: 'Subsets', contents: m('div#subsetTree')}
                    ]
                }),
                m("#rightpanelButtonBar", {style: {"width": "232px", "position": "absolute", "bottom": "5px"}},
                    [
                        m("button.btn.btn-default[id='buttonAddGroup'][type='button']", {
                                style: {
                                    "float": "left",
                                    "margin-left": "6px"
                                },
                                onclick: app.addGroup
                            },
                            "Group"
                        ),
                        m("button.btn.btn-default.ladda-button[data-spinner-color='#818181'][id='buttonDownload'][type='button']", {
                                style: {
                                    "float": "right",
                                    "margin-right": "6px",
                                    "data-style": "zoom-in"
                                },
                                onclick: app.download
                            },
                            m("span.ladda-label",
                                "Download"
                            )
                        )
                    ])
            ]
        })
    }

    view(vnode) {
        let {mode} = vnode.attrs;
        app.setOpMode(mode);

        return m('main',
            [
                this.header(mode),
                this.leftpanel(mode),
                this.rightpanel(mode),
                m("button#btnStage.btn.btn-default[type='button']", {
                    style: {
                        display: app.canvasKeySelected !== 'Custom' && app.opMode === 'subset' ? 'block' : 'none',
                        right: `calc(${common.panelOcclusion['right']} + 5px)`,
                        bottom: common.heightFooter + common.panelMargin + 6 + 'px',
                        position: 'fixed',
                        'z-index': 100
                    },
                    onclick: app.addRule
                }, "Stage"),
                m(Canvas, {
                    contents: [
                        m(CanvasActor, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Actor' ? 'block' : 'none'
                        }),
                        m(CanvasDate, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Date' ? 'block' : 'none'
                        }),
                        m(CanvasAction, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Action' ? 'block' : 'none'
                        }),
                        m(CanvasLocation, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Location' ? 'block' : 'none'
                        }),
                        m(CanvasCoordinates, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Coordinates' ? 'block' : 'none'
                        }),
                        m(CanvasCustom, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Custom' ? 'block' : 'none'
                        }),
                        m(CanvasPentaClass, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Penta Class' ? 'block' : 'none'
                        }),
                        m(CanvasRootCode, {
                            mode: mode,
                            display: app.canvasKeySelected === 'Root Code' ? 'block' : 'none'
                        })
                    ],
                    attrsAll: {style: mode === 'aggregate' ? {height: `calc(100% - ${heightHeader + heightFooter}px - ${tableHeight})`} : {}}
                }),
                m(TableAggregation, {mode: mode}),
                this.footer(mode)
            ]
        );
    }
}