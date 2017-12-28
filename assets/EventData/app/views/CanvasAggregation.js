export default class CanvasAggregation {
    view(vnode) {
        return (m("[id='aggregDataDisplay']", {
                style: {
                    "display": "inline-block",
                    "width": "100%",
                    "height": "75%",
                    "overflow-x": "auto",
                    "overflow-y": "scroll",
                    "white-space": "nowrap"
                }
            },
            [
                m("[id='aggregEventByPenta']", {style: {"display": "none"}},
                    [
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPentaAll.aggAllCheck[checked=''][id='aggregPentaAll'][name='aggregPentaAll'][type='checkbox'][value='all']"),
                                "All"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPenta[checked=''][id='aggregPenta0'][name='aggregPenta0'][type='checkbox'][value='penta0']"),
                                "Penta 0: Public Statement"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPenta[checked=''][id='aggregPenta1'][name='aggregPenta1'][type='checkbox'][value='penta1']"),
                                "Penta 1: Verbal Cooperation"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPenta[checked=''][id='aggregPenta2'][name='aggregPenta2'][type='checkbox'][value='penta2']"),
                                "Penta 2: Material Cooperation"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPenta[checked=''][id='aggregPenta3'][name='aggregPenta3'][type='checkbox'][value='penta3']"),
                                "Penta 3: Verbal Conflict"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkPenta[checked=''][id='aggregPenta4'][name='aggregPenta4'][type='checkbox'][value='penta4']"),
                                "Penta 4: Material Conflict"
                            ]
                        )
                    ]
                ),
                m("[id='aggregEventByRoot']", {style: {"display": "none"}},
                    [
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRootAll.aggAllCheck[checked=''][id='aggregRootAll'][name='aggregRootAll'][type='checkbox'][value='all']"),
                                "All"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot1'][name='aggregRoot1'][type='checkbox'][value='root1']"),
                                "Root 1: Make Public Statement"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot2'][name='aggregRoot2'][type='checkbox'][value='root2']"),
                                "Root 2: Appeal"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot3'][name='aggregRoot3'][type='checkbox'][value='root3']"),
                                "Root 3: Express Intent to Coop"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot4'][name='aggregRoot4'][type='checkbox'][value='root4']"),
                                "Root 4: Consult"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot5'][name='aggregRoot5'][type='checkbox'][value='root5']"),
                                "Root 5: Engage in Dip Coop"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot6'][name='aggregRoot6'][type='checkbox'][value='root6']"),
                                "Root 6: Engage in Material Aid"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot7'][name='aggregRoot7'][type='checkbox'][value='root7']"),
                                "Root 7: Provide Aid"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot8'][name='aggregRoot8'][type='checkbox'][value='root8']"),
                                "Root 8: Yield"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot9'][name='aggregRoot9'][type='checkbox'][value='root9']"),
                                "Root 9: Investigate"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot10'][name='aggregRoot10'][type='checkbox'][value='root10']"),
                                "Root 10: Demand"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot11'][name='aggregRoot11'][type='checkbox'][value='root11']"),
                                "Root 11: Disapprove"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot12'][name='aggregRoot12'][type='checkbox'][value='root12']"),
                                "Root 12: Reject"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot13'][name='aggregRoot13'][type='checkbox'][value='root13']"),
                                "Root 13: Threaten"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot14'][name='aggregRoot14'][type='checkbox'][value='root14']"),
                                "Root 14: Protest"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot15'][name='aggregRoot15'][type='checkbox'][value='root15']"),
                                "Root 15: Exhibit Force Posture"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot16'][name='aggregRoot16'][type='checkbox'][value='root16']"),
                                "Root 16: Reduce Relations"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot17'][name='aggregRoot17'][type='checkbox'][value='root17']"),
                                "Root 17: Coerce"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot18'][name='aggregRoot18'][type='checkbox'][value='root18']"),
                                "Root 18: Assault"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot19'][name='aggregRoot19'][type='checkbox'][value='root19']"),
                                "Root 19: Fight"
                            ]
                        ),
                        m(".separator"),
                        m("label.aggChkLbl",
                            [
                                m("input.aggChk.aggChkRoot[checked=''][id='aggregRoot20'][name='aggregRoot20'][type='checkbox'][value='root20']"),
                                "Root 20: Use Unconventional Mass Violence"
                            ]
                        )
                    ]
                )
            ]
        ));
    }
}