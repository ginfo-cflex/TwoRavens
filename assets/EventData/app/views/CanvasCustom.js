export default class CanvasCustom {
    oncreate(){
        // The editor menu for the custom subsets
        var editor = ace.edit("subsetCustomEditor");
        editor.$blockScrolling = Infinity;
        editor.session.setMode("ace/mode/json");

        editor.setOptions({
            readOnly: true,
            highlightActiveLine: false,
            highlightGutterLine: false
        });
        editor.renderer.$cursorLayer.element.style.opacity = 0;
        editor.textInput.getElement().disabled = true;
    }

    view(vnode) {
        return (m(".subsetDiv[id='subsetCustom']", {style: {"display": "none"}},
            [
                // Header
                m(".panel-heading.text-left[id='subsetCustomLabel']", {
                        style: {
                            "margin-top": "10px",
                            "width": "14em",
                            "float": "left"
                        }
                    },
                    m("h3.panel-title", "View Query String")
                ),
                // Show rightpanel query
                m("button.btn.btn-default[id='subsetCustomShowAll'][onclick='editor.setValue(JSON.stringify(buildSubset(subsetData), null, \'\t\'))']", {
                    style: {
                        "display": "inline",
                        "margin-top": "10px"
                    }
                }, "Show All"),

                // Ace editor
                m("pre[id='subsetCustomEditor']", {
                    style: {
                        "resize": "none",
                        "margin-left": "10px",
                        "margin-top": "5px",
                        "width": "calc(100% - 45px)",
                        "height": "calc(100% - 59px)"
                    }
                })
            ]
        ));
    }
}