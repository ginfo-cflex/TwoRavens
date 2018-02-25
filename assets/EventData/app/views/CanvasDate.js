import m from 'mithril';
import {setupDate, setDatefromSlider} from "../subsets/Date.js"
import {panelMargin} from "../../../common/app/common";

export default class CanvasDate {
    oncreate() {
        setupDate();
    }

    view(vnode) {
        let {display} = vnode.attrs;
        return (m("#canvasDate.subsetDiv", {style: {"display": display, 'padding-top': panelMargin + 'px'}},
            [
                m("[id='dateSVGdiv']", {style: {"display": "inline-block"}}, m("svg[height='500'][id='dateSVG'][width='500']")),
                m("div",
                    {
                        style: {
                            "display": "inline-block",
                            "vertical-align": "top",
                            "width": "20%",
                            "margin": "20px"
                        }
                    },
                    [
                        m("[id='dateOptions']",
                            m(".form-group[id='dateInterval']",
                                [
                                    // Set date from slider
                                    m("button.btn.btn-default[type='button']", {
                                        style: {
                                            "margin-top": "10px",
                                            "text-align": "center"
                                        },
                                        onclick: function (e) {
                                            setDatefromSlider();
                                            e.redraw = false;
                                        }
                                    }, "Bring Date from Slider"),

                                    // From date
                                    m("label[for='fromdate'][id='dateFromLab']", {
                                        style: {
                                            "text-align": "left",
                                            "width": "100%",
                                            "margin-top": "10px"
                                        }
                                    }, "From:"),
                                    m("input.form-control[id='fromdate'][type='text']"),

                                    // To date
                                    m("label[for='todate'][id='dateToLab']", {
                                        style: {
                                            "text-align": "left",
                                            "width": "100%",
                                            "margin-top": "10px"
                                        }
                                    }, "To:"),
                                    m("input.form-control[id='todate'][type='text']")
                                ]
                            )
                        ),
                        m("[id='dateAggregOption']", {style: {"display": "none"}},
                            [
                                m("button.dateAggregIntBtn[id='dateNone']", "None"),
                                m("input.aggregDateChk[id='aggregDateNone'][type='checkbox'][value='none']"),

                                m(".separator"),
                                m("button.dateAggregIntBtn[id='dateWeek']", "Weekly"),
                                m("input.aggregDateChk[id='aggregDateWeek'][type='checkbox'][value='week']"),

                                m(".separator"),
                                m("button.dateAggregIntBtn[id='dateMonth']", "Monthly"),
                                m("input.aggregDateChk[id='aggregDateMonth'][type='checkbox'][value='month']"),

                                m(".separator"),
                                m("button.dateAggregIntBtn[id='dateQuarter']", "Quarterly"),
                                m("input.aggregDateChk[id='aggregDateQuarter'][type='checkbox'][value='quarter']"),

                                m(".separator"),
                                m("button.dateAggregIntBtn[id='dateYear']", "Yearly"),
                                m("input.aggregDateChk[id='aggregDateYear'][type='checkbox'][value='year']"),
                                m(".separator")
                            ]
                        )
                    ]
                )
            ]
        ));
    }
}
