/*
 *  Datamart UI component and API calls
 */
import m from 'mithril';

import * as app from "../app";
import * as common from "../../common/common";
import JSONSchema from "../../common/views/JSONSchema";
import Button from "../../common/views/Button";
import ButtonPlain from "../../common/views/ButtonPlain";
import Table from "../../common/views/Table";
import ListTags from "../../common/views/ListTags";
import ButtonRadio from "../../common/views/ButtonRadio";
import ModalVanilla from "../../common/views/ModalVanilla";
import PanelList from "../../common/views/PanelList";
import TextField from "../../common/views/TextField";
import Dropdown from "../../common/views/Dropdown";
import Icon from "../../common/views/Icon";
import TwoPanel from "../../common/views/TwoPanel";
import Checkbox from "../../common/views/Checkbox";
import Popper from "../../common/views/Popper";

import * as queryMongo from '../manipulations/queryMongo';

import ButtonLadda from "../views/LaddaButton";
import {numberWithCommas} from '../utils';
import {bold} from "../index";

import {datamartQueryInputSchema} from "./query_input_schema_2019_06";
import {datamartDatasetIndexSchema} from "./dataset_schema_2019_01";


let setDefault = (obj, id, value) => obj[id] = id in obj ? obj[id] : value;
let warn = value => m('[style=color:#dc3545;display:inline-block;margin-right:1em;]', value);

let makeCard = ({key, color, summary, labelWidth}) => m('table', {
        style: {
            'background': common.menuColor,
            // 'border': common.borderColor,
            margin: '1em',
            'box-shadow': '0px 5px 5px rgba(0, 0, 0, .2)',
            width: 'calc(100% - 2em)'
        }
    },
    m('tr',
        m('td', {
            style: {
                background: color,
                height: '100%',
                padding: '1em',
                width: labelWidth || 0, // by default, 0 makes div width wrap content
                'max-width': labelWidth || 0,
                'word-break': 'break-word',
                'border-right': common.borderColor
            }
        }, bold(key)),
        m('td', {style: {width: 'calc(100% - 2em)'}}, summary))
);

let makeDatasetCard = (preferences, result, index, manipulations, endpoint, labelWidth) => {

    let getData = preferences.getData;
    let cached = preferences.cached;

    /**
     *  Materialize: download a dataset based on a specific search result
     */
    let materializeData = async () => {
        console.log(`materializeData ${index}`);
        // Get the search result and find the dataset id.
        //  May differ between Datamart.  See "infoPaths" above
        //
        let id = getData(result, 'id');
        preferences.selectedResult = result;

        if (!(id in cached)) {
            preferences.setPreviewButtonState(index, true);
            let sourceMode = preferences.sourceMode;

            // Use the materialize endpoint.
            //  Note: the materialized data is returned through websockets
            //    but an intial message relays if the request worked.
            //
            let response = await m.request(endpoint + 'materialize-async', {
                method: 'POST',
                data: {
                    search_result: JSON.stringify(preferences.selectedResult),
                    source: preferences.sourceMode
                    //  workspace_id: app.workspace.user_workspace_id
                    //problem_id:
                }
            });
            if (response.success) {
                preferences.success[sourceMode] = 'Preview initiated ...';
                delete preferences.error[sourceMode];
            } else {
                // show the response error message
                delete preferences.success[sourceMode];
                preferences.error[sourceMode] = response.message;
            }
        }

        m.redraw();
    };


    /*
     * For each result, dynamically define the "Augment"
     **/
    let buttonAugment = m(Button, {
        style: {'margin': '0em 0.25em'},
        onclick: async () => {

            preferences.selectedResult = result;

            // set suggested pairs to join on automatically
            let augmentationData = getData(result, 'augmentation');

            // NYU
            if ('left_columns_names' in augmentationData){
                preferences.joinPairs = augmentationData.left_columns_names.map((_, j) => [
                    augmentationData.left_columns_names[j],
                    augmentationData.right_columns_names[j]
                ]);
              } else if ('left_columns' in augmentationData) {
                  // --- ISI ---('left_columns' in augmentationData) {
                let originalLeftColumns = [...queryMongo.buildPipeline(
                    manipulations, app.workspace.raven_config.variablesInitial)['variables']];

                preferences.joinPairs = augmentationData.left_columns.map((_, j) => [
                    augmentationData.left_columns[j][0].map(colIndex => originalLeftColumns[colIndex]),
                    [result.summary["Recommend Join Columns"]]
                ]);
            }

            if (preferences.sourceMode === 'ISI')
                preferences.modalShown = 'augment';

            if (preferences.sourceMode === 'NYU')
                preferences.modalShown = 'augment';

        }
    }, 'Augment');

    let buttonMetadata = m(Button, {
        style: {'margin': '0em 0.25em'},
        onclick: () => {
            preferences.selectedResult = result;
            preferences.modalShown = 'metadata';
        }
    }, 'Metadata');

    let buttonPreview = m(ButtonLadda, {
        id: 'buttonPreview' + index,
        class: 'btn btn-secondary',
        activeLadda: preferences.previewButtonState[index],
        //disabled: preferences.previewButtonState[i] === true,

        style: {'margin': '0em 0.25em', 'data-spinner-color': 'black', 'data-style': 'zoom-in'},
        onclick: async () => {
            let id = getData(result, 'id');
            preferences.selectedResult = result;

            await materializeData();

            if (id in cached)
                preferences.modalShown = 'preview';

            m.redraw();
        }
    }, 'Preview');

    return makeCard({
        key: m('', m('', getData(result, 'name') || ''),
            m('p[style=font-weight:normal]', `(#${index + 1})`)),
        color: preferences.selectedResult === result ? common.selVarColor : common.grayColor,
        summary: m('div',
            m('label[style=width:100%]', 'Relevance: ' + getData(result, 'score')),
            buttonPreview,
            buttonAugment,
            buttonMetadata,
            m(Table, {
                attrsAll: {style: {'margin-top': '.5em'}},
                data: [
                    (getData(result, 'description') || '').length > 0 && [
                        'Description', getData(result, 'description')
                    ],
                    getData(result, 'size') && [
                        'Size (bytes)', numberWithCommas(getData(result, 'size'))
                    ],
                    getData(result, 'row count') && [
                        'Size (rows)', getData(result, 'row count')
                    ],
                    getData(result, 'keywords') && [
                        'Keywords', m(ListTags, {tags: getData(result, 'keywords'), readonly: true})
                    ]
                ]
            })),
        labelWidth
    })
};

export class CanvasDatamart {
    view(vnode) {
        return m('div#canvasDatamart', {
                style: {'height': '100%', 'width': '100%', 'padding-top': common.panelMargin}
            },
            m('div', {
                style: {
                    'max-width': '1200px',
                    'margin': 'auto',
                    'height': '100%'
                }
            }, m(Datamart, vnode.attrs)))
    }
}

export class Datamart {

    view(vnode) {
        let {
            preferences,
            manipulations,
            dataPath,   // where to load data from, to augment with
            labelWidth, // width of titles on left side of cards
            endpoint,   // Django app url
        } = vnode.attrs;

        let {
            hints,    // keyword hints passed from query schema
            query,    // https://datadrivendiscovery.org/wiki/display/work/Datamart+Query+API
            results,  // list of matched metadata
            indices,  // data to be attached to the upload
            getData
        } = preferences;

        // clear out cached results if the dataPath has changed since the previous search
        if (dataPath !== preferences.cachedDataPath) {
            Object.keys(results).forEach(source => results[source].length = 0);
            preferences.cached = {};
            preferences.success = {};
            preferences.error = {};
        }

        // ISI doesn't support augmenting without a dataset
        if (preferences.sourceMode === 'ISI') preferences.includeDataset = true;

        if (preferences.isAugmenting) return m('div',
            m('h5', 'The system is performing an augmentation.'),
            common.loader('DatamartAugmenting')
        );

        let handleIndex = async index => {
            console.log('Datamart Index:', index);

            // preserve state after async is awaited
            let sourceMode = preferences.sourceMode;

            let response = await m.request(endpoint + 'get_metadata', {
                method: 'POST',
                data: {
                    custom: JSON.stringify(index),
                    source: sourceMode
                }
            });

            if (response.success) {
                delete preferences.error[sourceMode];
                preferences.indices.length = 0;
                preferences.indices.push(...response.data);
                preferences.success[sourceMode] = `Found ${response.data.length} potential dataset${response.data.length === 1 ? '' : 's'}. Please review the details.`
                console.log("#debug after submission of new dataset, response.data");
                console.log(response.data);
            } else {
                preferences.error[sourceMode] = response.message;
                delete preferences.success[sourceMode]
            }
        };

        return m('div', {style: {width: '100%',
                height: '100%',
                position: 'relative'
        }},
            m(TwoPanel, {
                left: [
                    preferences.error[preferences.sourceMode] && m('div#errorMessage', {
                        style: {
                            background: 'rgba(0,0,0,.05)',
                            'border-radius': '.5em',
                            'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                            margin: '10px 0',
                            padding: '1em'
                        }
                    }, [
                        m('div', {
                            style: {display: 'inline-block'},
                            onclick: () => delete preferences.error[preferences.sourceMode]
                        }, m(Icon, {name: 'x'})),
                        m('div', {style: {'margin-left': '1em', display: 'inline-block'}},
                            warn('Error:'), preferences.error[preferences.sourceMode])
                    ]),

                    preferences.success[preferences.sourceMode] && m('div#successMessage', {
                        style: {
                            background: 'rgba(0,0,0,.05)',
                            'border-radius': '.5em',
                            'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                            margin: '10px 0',
                            padding: '1em'
                        }
                    }, [
                        m('div#successMessage', {
                            style: {display: 'inline-block'},
                            onclick: () => delete preferences.success[preferences.sourceMode]
                        }, m(Icon, {name: 'x'})),
                        m('div', {style: {'margin-left': '1em', display: 'inline-block'}},
                            preferences.success[preferences.sourceMode])
                    ]),

                    preferences.datamartMode === 'Search' && [

                        (hints || []).length > 0 && [
                            m('h4', 'Suggested Keywords'),
                            m('div[style=margin:.5em]', 'Click on a keyword to add it to the list of keywords in the query.'),
                            m(Table, {
                                data: hints.map(row => ({
                                    Domain: row.domain,
                                    Keywords: m(ListTags, {
                                        readonly: true,
                                        tags: row.keywords
                                            .filter(key => !(preferences.query.keywords || []).includes(key))
                                            .map(key => m('div', {
                                                onclick: () => {
                                                    if (!('keywords' in preferences.query)) preferences.query.keywords = [];
                                                    if (preferences.query.keywords.includes(key)) preferences.query.keywords.splice(key, 1)
                                                    else preferences.query.keywords.push(key)

                                                    // auto-searching
                                                    clearTimeout(preferences.searchTimeout);
                                                    if (!preferences.includeDataset)
                                                        preferences.searchTimeout = setTimeout(() => search(preferences, endpoint, dataPath, preferences.includeDataset, preferences.includeQuery), 500)
                                                }
                                            }, key))
                                    })
                                }))
                            })
                        ],

                        m('h4', 'Query'),
                        m('div[style=margin:.5em]', 'Show datasets that match keywords, or contain variables with temporal or geospatial attributes.'),
                        m(`div[style=background:${common.menuColor}]`, {
                            onkeyup: e => {
                                // auto-searching
                                clearTimeout(preferences.searchTimeout);
                                if (e.code === 'Enter')
                                    search(preferences, endpoint, dataPath, preferences.includeDataset, preferences.includeQuery);
                                else if (!preferences.includeDataset && e.code !== 'Tab'){
                                    preferences.searchTimeout = setTimeout(() => search(preferences, endpoint, dataPath, preferences.includeDataset, preferences.includeQuery), 500);
                                }
                            }
                        }, m(JSONSchema, {
                            data: query,
                            schema: datamartQueryInputSchema
                        })),

                        m(ButtonRadio, {
                            id: 'dataSourceButtonBar',
                            onclick: state => {
                                preferences.sourceMode = state;
                                preferences.selectedResult = undefined;
                            },
                            activeSection: preferences.sourceMode,
                            sections: [{value: 'NYU'}, {value: 'ISI'}],
                            attrsAll: {style: {margin: '1em', width: 'auto'}},
                            attrsButtons: {style: {width: 'auto'}}
                        }),

                        m('div', {style: {float: 'right'}},
                            m('div', {
                                    style: {margin: '1.5em', display: 'inline-block'},
                                    onclick: () => preferences.includeQuery = !preferences.includeQuery
                                },
                                m('label[style=margin:.25em;font-weight:bold]', 'Use query in search '),
                                m(Checkbox, {
                                    checked: preferences.includeQuery
                                })),
                            m('div', {
                                    style: {margin: '1.5em', display: 'inline-block'},
                                    onclick: () => preferences.includeDataset = !preferences.includeDataset
                                },
                                m('label[style=margin:.25em;font-weight:bold]',
                                    m(Popper, {
                                        content: () => m('div[style=max-width:22em]',
                                            'Check to show datasets that are considered joinable with the current dataset, ',
                                            m('pre[style=display:inline]', app.workspace.d3m_config.name), '.')
                                        }, 'Use dataset in search ')),
                                m(Checkbox, {
                                    checked: preferences.includeDataset
                                })),
                        /*
                         * Start: Datamart Search Call
                         */
                            m(Button, {
                                    style: {float: 'right', margin: '1em'},
                                    disabled: preferences.isSearching[preferences.sourceMode],
                                    onclick: () => search(preferences, endpoint, dataPath, preferences.includeDataset, preferences.includeQuery)
                                },
                                'Search')), // Datamart Search Call

                    ],
                    preferences.datamartMode === 'Index' && [
                        m('div', {style: {margin: '1em'}}, 'Indexing is for adding your own datasets to datamart. You may provide a ', bold('link'), ' to a file, or ', bold('scrape'), ' datasets from a website.'), // You may upload a file or extract data from a link.
                        m(ButtonRadio, {
                            id: 'datamartIndexMode',
                            onclick: state => preferences.datamartIndexMode = state,
                            activeSection: preferences.datamartIndexMode,
                            sections: [
                                // {value: 'File'},
                                {value: 'Link'},
                                {value: 'Scrape'}
                            ]
                        }),
                        preferences.datamartIndexMode === 'File' && [
                            m('label.btn.btn-default.btn-file', {style: {margin: '1em', display: 'inline-block'}}, [
                                m('input', {
                                    hidden: true,
                                    type: 'file',
                                    style: {display: 'none'},
                                    onchange: async e => {

                                        // preserve state after async is awaited
                                        let sourceMode = preferences.sourceMode;

                                        let file = e.target.files[0];

                                        let data = new FormData();
                                        data.append("source_file", file);

                                        // initial upload
                                        let response = await m.request({
                                            method: "POST",
                                            url: endpoint + "upload",
                                            data: data
                                        });

                                        if (!response.success) {
                                            preferences.error[sourceMode] = response.message;
                                            return;
                                        }
                                    }
                                })
                            ], 'Browse')
                        ],

                        preferences.datamartIndexMode === 'Link' && [
                            m(TextField, {
                                style: {margin: '1em', width: 'calc(100% - 15em)', display: 'inline-block'},
                                id: 'datamartLinkTextField',
                                value: preferences.indexLink,
                                placeholder: 'Url to file',
                                oninput: value => preferences.indexLink = value,
                                onblur: value => preferences.indexLink = value
                            }),
                            m('div', {style: {margin: '1em', 'margin-left': '0px', display: 'inline-block'}}, m(Dropdown, {
                                id: 'fileTypeDropdown',
                                items: ['csv', 'excel'],
                                activeItem: preferences.indexFileType,
                                onclickChild: value => preferences.indexFileType = value
                            })),
                            m(Button, {
                                style: {
                                    float: 'right',
                                    margin: '1em',
                                    'margin-left': '0px',
                                    'max-width': '10em',
                                    display: 'inline-block'
                                },
                                onclick: () => handleIndex({
                                    materialization_arguments: {
                                        url: preferences.indexLink,
                                        file_type: preferences.indexFileType
                                    }
                                })
                            }, 'Submit')
                        ],

                        preferences.datamartIndexMode === 'Scrape' && [
                            m(TextField, {
                                style: {margin: '1em', width: 'calc(100% - 10em)', display: 'inline-block'},
                                id: 'datamartScrapeTextField',
                                value: preferences.indexScrape,
                                placeholder: 'Url to webpage with tables',
                                oninput: value => preferences.indexScrape = value,
                                onblur: value => preferences.indexScrape = value
                            }),
                            m(Button, {
                                style: {float: 'right', margin: '1em', 'max-width': '10em', display: 'inline-block'},
                                onclick: () => handleIndex({
                                    materialization_arguments: {
                                        url: preferences.indexScrape,
                                        file_type: 'html'
                                    }
                                })
                            }, 'Submit')
                        ],

                        indices.map(index => m(`div[style=background:${common.menuColor}]`, m(JSONSchema, {
                            data: index,
                            schema: datamartDatasetIndexSchema
                        }))),

                        indices.length > 0 && m(Button, {
                            onclick: async () => {
                                // preserve state after async is awaited
                                let sourceMode = preferences.sourceMode;

                                let responses = [];
                                let promises = indices.map((index, i) => m.request(endpoint + 'index', {
                                    method: 'POST',
                                    data: {
                                        index: JSON.stringify(index),
                                        source: sourceMode
                                    }
                                }).then(response => responses[i] = response));

                                await Promise.all(promises);

                                preferences.success[sourceMode] = 'Index ' + responses
                                    .reduce((out, response, i) => response.success ? [...out, i] : out, []).join(', ') + ' successful.';

                                preferences.indices = indices.filter((index, i) => !responses[i].success);

                                if (preferences.indices.length) {
                                    console.log("#debug responses");
                                    console.log(responses);
                                    preferences.error[sourceMode] = 'Some datasets failed uploading to datamart. The failed datasets are listed below.';
                                    delete preferences.success[sourceMode]
                                } else
                                    preferences.success[sourceMode] = `Dataset${responses.length === 1 ? '' : 's'} successfully indexed.`

                                m.redraw()
                            }
                        }, 'Submit')
                    ]
                ],
                right: [

                    preferences.isSearching[preferences.sourceMode] && common.loader('DatamartSearching'),

                    m('div#datamartResults', results[preferences.sourceMode]
                        .sort((a, b) => getData(b, 'score') - getData(a, 'score'))
                        .map((result, i) => makeDatasetCard(preferences, result, i, manipulations, endpoint, labelWidth))
                    )
                ]
            }),
            // m(ButtonRadio, {
            //     id: 'datamartButtonBar',
            //     onclick: state => preferences.datamartMode = state,
            //     activeSection: preferences.datamartMode,
            //     sections: [{value: 'Search'}, {value: 'Index'}]
            // }),

        )
    }
}


// additional menus for displaying tables, augment columns and metadata
export class ModalDatamart {
    view(vnode) {
        let {
            preferences,
            endpoint,
            manipulations
        } = vnode.attrs;

        let {
            cached, // summary info and paths related to materialized datasets
            getData,
            selectedResult
        } = preferences;

        if (!getData || !preferences.modalShown)
            return;

        return getData && preferences.modalShown && m(ModalVanilla, {
            id: 'datamartModal',
            setDisplay: () => preferences.modalShown = false
        }, [
            preferences.modalShown === 'preview' && [
                m('h4',
                    (preferences.getData(selectedResult, 'name') || '') + ' Preview',
                    m(Button, {
                        class: 'btn-sm',
                        style: {'margin-left': '1em'},
                        id: 'btnDownload',
                        onclick: () => app.downloadFile(cached[preferences.getData(selectedResult, 'id')].data_path)
                    }, 'Download')),
                m('div', {style: {width: '100%', overflow: 'auto'}},
                    m(Table, {
                        headers: cached[preferences.getData(selectedResult, 'id')].data_preview[0],
                        data: cached[preferences.getData(selectedResult, 'id')].data_preview.slice(1)
                    }))
            ],

            preferences.modalShown === 'metadata' && [
              m('h4', (getData(selectedResult, 'name') || '') + ' Metadata'),
              m('label[style=width:100%]', 'Score: ' + getData(selectedResult, 'score') || 0),
              m('div[style=width:100%;overflow:auto]',
                m(Table, {
                    data: getData(selectedResult, 'data'),
                    // attrsCells: {'class': 'text-left'}, // {class: "text-left"},
                  }
                ),
              ),
            ],


            preferences.modalShown === 'augment' && [

                // Error messages at the top
                //
                preferences.error[preferences.sourceMode] && m('div#errorMessage', {
                    style: {
                        background: 'rgba(0,0,0,.05)',
                        'border-radius': '.5em',
                        'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                        margin: '10px 0',
                        padding: '1em'
                    }
                }, [
                    m('div', {
                        style: {display: 'inline-block'},
                        onclick: () => delete preferences.error[preferences.sourceMode]
                    }, m(Icon, {name: 'x'})),
                    m('div', {style: {'margin-left': '1em', display: 'inline-block'}},
                        warn('Error:'), preferences.error[preferences.sourceMode])
                ]),

                // Instructions
                //
                // Listing of pairs
                //
                m('h4', 'Augment / Join Datasets'),


                m('div',
                    !preferences.joinPairs.length && m('div',
                      m('p', 'Clicking "Augment" will join your dataset with the found dataset.'),
                      m('p', bold('First'), ' please choose variables from each dataset to connect them. See ', bold("Variable Pairs"), ' below.'),
                    ),

                    preferences.joinPairs.length > 0 && m('div',
                      m('p', 'Click "Augment" to join your dataset with the found dataset.'),
                    ),

                    // List of join pairs
                    //
                    preferences.joinPairs.map((pair, i) => m('div#pairContainer' + i, {
                        style: {
                            background: 'rgba(0,0,0,.05)',
                            'border-radius': '.5em',
                            'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                            margin: '.5em 0 1em 0',
                            padding: '1em'
                        }
                    }, [
                        m('div', {
                            style: {display: 'inline-block'},
                            onclick: () => {
                                if (preferences.isAugmenting || preferences.sourceMode === 'ISI') return;
                                preferences.joinPairs.splice(preferences.joinPairs.findIndex(elem => elem === pair), 1)
                            }
                        }, !preferences.isAugmenting && preferences.sourceMode !== 'ISI' && m(Icon, {name: 'x'})),
                        m('div', {style: {'margin-left': '1em', display: 'inline-block'}},
                        preferences.sourceMode === 'ISI' ? `ISI join with [${pair[1].join(', ')}]` : `Joining [${pair[0].join(', ')}] with [${pair[1].join(', ')}]`)

                    ])),
                    // end: list join pairs

                    m(ButtonLadda, {
                        id: 'augmentButton',
                        style: {margin: '1em'},
                        activeLadda: preferences.isAugmenting,

                        class: `${(!preferences.joinPairs.length || preferences.isAugmenting === true) ? 'btn-default' : 'btn-success active'}`,
                        disabled: !preferences.joinPairs.length || preferences.isAugmenting === true,

                        onclick: async () => {
                            if (app.workspace.is_original_workspace) {
                                app.setNewWorkspaceName(app.workspace.d3m_config.name + '-before-augment');
                                app.saveAsNewWorkspace();
                            }

                            preferences.isAugmenting = true;

                            let sourceMode = preferences.sourceMode;

                            let originalLeftColumns = [...queryMongo.buildPipeline(
                                manipulations, app.workspace.raven_config.variablesInitial)['variables']];


                            let augment_api_data = {
                                data_path: preferences.cachedDataPath,
                                search_result: JSON.stringify(preferences.selectedResult),
                                source: preferences.sourceMode,
                                exact_match: preferences.exactMatch
                            };

                            let summary = {};
                            if (preferences.sourceMode === 'NYU') {
                                // For ISI, this "preferences.selectedResult.metadata.columns" is
                                //    "preferences.selectedResult.metadata.variables"
                                let originalRightColumns = preferences.selectedResult.metadata.columns.map(row => row.name);

                                let joinLeftColumns = [];
                                let joinRightColumns = [];

                                preferences.joinPairs.forEach(pair => {
                                    joinLeftColumns.push(pair[0]
                                        .map(leftCol => originalLeftColumns.indexOf(leftCol)));
                                    joinRightColumns.push(pair[1]
                                        .map(rightCol => originalRightColumns.indexOf(rightCol)));
                                });
                                summary.joinPairs = preferences.joinPairs;
                                Object.assign(augment_api_data, {
                                    left_columns: JSON.stringify(joinLeftColumns),
                                    right_columns: JSON.stringify(joinRightColumns)
                                });
                            }

                            // standardized summary of some key fields
                            summary = ['name', 'description', 'row count'].reduce((out, key) => Object.assign(out, {
                                [key]: preferences.getData(preferences.selectedResult, key)
                            }), summary);
                            augment_api_data.summary = summary;

                            console.log('augment_api_data: ' + JSON.stringify(augment_api_data));

                            let response = await m.request(endpoint + 'augment', {
                                method: 'POST',
                                data: augment_api_data
                            });

                            if (response.success) {
                                delete preferences.error[sourceMode];
                                preferences.success[sourceMode] = response.message;
                                preferences.success[sourceMode] = '';
                                preferences.modalShown = false;
                            } else {
                              /*setModal(m('div', m('p', 'An error occurred:'),
                                  m('p', response.data)),
                                  "Sorry Augment Failed",
                                  true,
                                  "Close",
                                  true);*/
                                preferences.error[sourceMode] = m.trust(response.message);
                                // turn off spinner
                                preferences.isAugmenting = false;
                                delete preferences.success[sourceMode];
                                m.redraw()
                                // preferences.isSearching[sourceMode] = false;
                            }

                            console.log("#debug response augment");
                            console.log(response);
                        }
                    }, 'Augment'),
                  ),
                    //m('p', "Please choose the variables to connect your dataset" +
                    //       " from the found dataset.")),


                preferences.sourceMode !== 'ISI' && [
                    // Listing of pairs
                    //
                    m('hr'),
                    m('h4', 'Variable Pairs'),

                    m('div', [
                        !preferences.joinPairs.length && [
                            m('p', '(No pairs selected)'),
                            m('p', 'Please connect the datasets using variables:')],
                        preferences.joinPairs.length > 0 && m('p', '(Optional) To add another Variable Pair:'),
                        m('ol', [
                            m('li', 'Click on a variable from the ', bold("Your Dataset Variables"), ' column.'),
                            m('li', 'Select a variable from the ', bold("Found Dataset Variables"), ' column.'),
                            m('li', 'Click ', bold("Add Pairing"), '.  If desired, add additional pairings.'),
                            m('li', 'Click ', bold("Augment")),
                        ]),
                    ]),

                    m('div', [
                        m(ButtonPlain, {
                            style: {margin: '1em 1em .5em 1em'},
                            title: 'supply variables from both the left and right datasets',

                            class: `${(!preferences.leftJoinVariables.size || !preferences.rightJoinVariables.size || preferences.isAugmenting === true) ? 'btn-default' : 'btn-success active'}`,

                            disabled: !preferences.leftJoinVariables.size || !preferences.rightJoinVariables.size || preferences.isAugmenting === true,
                            onclick: () => {
                                if (!preferences.leftJoinVariables.size || !preferences.rightJoinVariables.size || preferences.isAugmenting)
                                    return;

                                preferences.joinPairs.push([
                                    [...preferences.leftJoinVariables],
                                    [...preferences.rightJoinVariables]]);

                                preferences.leftJoinVariables = new Set();
                                preferences.rightJoinVariables = new Set();

                            }
                        }, 'Add Pairing'),
                    ]),


                    m('hr'),

                    m('h4[style=width:calc(50% - 1em);display:inline-block]', 'Your Dataset Variables'),
                    m('h4[style=width:calc(50% - 1em);display:inline-block]', 'Found Dataset Variables'),

                    m('div', {style: {width: 'calc(50% - 1em)', display: 'inline-block', 'vertical-align': 'top'}},
                        m(PanelList, {
                            id: 'leftColumns',
                            items: [...queryMongo.buildPipeline(
                                manipulations, app.workspace.raven_config.variablesInitial)['variables']],
                            colors: {
                                [app.hexToRgba(preferences.isAugmenting ? common.grayColor : common.selVarColor)]:
                                    [...preferences.leftJoinVariables]
                            },
                            callback: variable => {
                                if (preferences.isAugmenting) return;
                                preferences.leftJoinVariables.has(variable)
                                    ? preferences.leftJoinVariables.delete(variable)
                                    : preferences.leftJoinVariables.add(variable);
                                setTimeout(m.redraw, 1000);
                            },
                            attrsAll: {
                                style: {
                                    background: 'rgba(0,0,0,.025)',
                                    'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                                    'max-width': '30em',
                                    padding: '1em',
                                    margin: 'auto'
                                }
                            }
                        })),
                    m('div', {style: {width: 'calc(50% - 1em)', display: 'inline-block', 'vertical-align': 'top'}},
                        m(PanelList, {
                            id: 'rightColumns',
                            items: selectedResult.metadata.columns.map(variable => variable.name),
                            colors: {
                                [app.hexToRgba(preferences.isAugmenting ? common.grayColor : common.selVarColor)]: [...preferences.rightJoinVariables]
                            },
                            callback: variable => {
                                if (preferences.isAugmenting) return;
                                preferences.rightJoinVariables.has(variable)
                                    ? preferences.rightJoinVariables.delete(variable)
                                    : preferences.rightJoinVariables.add(variable);
                                setTimeout(m.redraw, 1000);
                            },
                            attrsAll: {
                                style: {
                                    background: 'rgba(0,0,0,.025)',
                                    'box-shadow': '0px 5px 10px rgba(0, 0, 0, .1)',
                                    'max-width': '30em',
                                    padding: '1em',
                                    margin: 'auto'
                                }
                            }
                        }))
                ]

            ]
        ])
    }
}

export let search = async (preferences, endpoint, dataPath, includeDataset=true, includeQuery=true) => {

    // preserve state after async is awaited
    let sourceMode = preferences.sourceMode;
    preferences.results[sourceMode].length = 0;

    if ((preferences.query.keywords || []).length === 0 && (preferences.query.variables || []).length === 0)
        includeQuery = false;

    if (!includeDataset && !includeQuery){
        preferences.showDatamartErrorMsg(sourceMode, "Either a dataset or query must be included to search.");
        return;
    }

    // results are tied to data from dataPath
    preferences.cachedDataPath = dataPath;

    // enable spinner
    preferences.isSearching[sourceMode] = true;
    m.redraw();

    let searchParams = {source: sourceMode, dataset_path: dataPath};
    if (includeQuery) searchParams.query = JSON.stringify(preferences.query);

    if (includeDataset) {
        let response = await m.request(endpoint + 'search-by-dataset', {
            method: 'POST',
            data: searchParams
        });
        if (response.success) {
            preferences.showDatamartSuccessMsg(sourceMode, response.message);
        } else {
            preferences.isSearching[sourceMode] = false;

            preferences.showDatamartErrorMsg(sourceMode, response.message);
        }
    }

    else {
        let response = await m.request(endpoint + 'search', {
            method: 'POST',
            data: searchParams
        });
        preferences.handleSearchResults(sourceMode, response);
    }
};

export let setDatamartDefaults = preferences => {

    // access information from NYU/ISI responses along these paths
    setDefault(preferences, 'infoPaths', {
        'NYU': {
            'id': ['id'],
            'row count': ['metadata', 'nb_rows'],
            'name': ['metadata', 'name'],
            'augmentation': ['augmentation'],
            'score': ['score'],
            'description': ['metadata', 'description'],
            'size': ['metadata', 'size'],
            'keywords': undefined,
            'data': ['metadata']
        },
        'ISI': {
            'id': ['id'],
            'row count': ['metadata', 0, 'metadata', 'dimension', 'length'], // TODO: don't implicitly count on the rowcount being first
            'name': undefined,
            'augmentation': ['augmentation'],
            'score': ['score'],
            'description': ['summary', 'title'],
            'size': undefined,
            'keywords': undefined,
            'data': ['metadata']
        }
    });

    // Define the "getData" function
    //
    setDefault(preferences, 'getData', (result, attribute) => {
        let path = preferences.infoPaths[preferences.sourceMode][attribute];
        return path && path.reduce((out, term) => term in out && out[term], result)
    });

    // Show a datamart error message -- datamart specific
    //
    setDefault(preferences, 'showDatamartErrorMsg', (datamartSource, message) => {

        // remove any success messages
        delete preferences.success[datamartSource]; // remove "success"

        // show the error message
        preferences.error[datamartSource] =  m('b', {class: "h5"}, message);

        m.redraw();

    });

    // Show a datamart success message -- datamart specific
    //
    setDefault(preferences, 'showDatamartSuccessMsg', (datamartSource, message) => {

        // remove any success messages
        delete preferences.error[datamartSource]; // remove "success"

        // show the error message
        preferences.success[datamartSource] =  m('b', {class: "h5"}, message);

        m.redraw();
    });

    setDefault(preferences, 'handleSearchResults', (datamartSource, response) => {

        // stop any UI search indicators
        preferences.isSearching[datamartSource] = false;

        console.log('datamartSource: ' + datamartSource);
        console.log('response.success: ' + response.success);

        /* -------------------------------------------
         * Search failed, show message
         * ------------------------------------------- */
        if (!response.success) {
            console.log('response: ' + JSON.stringify(response));
            preferences.showDatamartErrorMsg(datamartSource, response.message);
            return;
        }

        /* -------------------------------------------
         * Search success, show results
         * ------------------------------------------- */
        console.log('results are back! ' + JSON.stringify(response));
        // (moved sort to server side)
        // clear array and add results
        preferences.results[datamartSource].length = 0;
        preferences.results[datamartSource].push(...response.data);

        let numResults = preferences.results[datamartSource].length
        console.log('Num results: ' + numResults);

        if (numResults === 0) {
            // No datasets found
            delete preferences.success[datamartSource]; // remove "success"
            preferences.error[datamartSource] = 'No datasets found.';

        } else {
            // Datasets found!
            delete preferences.error[datamartSource]; // remove error

            let numDatasetMsg = '';
            if (numResults === 0){
                numDatasetMsg = 'Sorry! No datasets found.';
            } else if (numResults === 1){
                numDatasetMsg = '1 dataset found.';
            } else {
                numDatasetMsg += `${numResults} datasets found.`;
            }

            preferences.showDatamartSuccessMsg(datamartSource, numDatasetMsg);

            console.log('msg: ' + numDatasetMsg);
        }
    });

    setDefault(preferences, 'includeDataset', true);
    setDefault(preferences, 'includeQuery', true);

    setDefault(preferences, 'setPreviewButtonState', (idx, val) => {
        preferences.previewButtonState[idx] = val;
    });

    setDefault(preferences, 'previewButtonState', {});

    // set default menu state
    setDefault(preferences, 'datamartMode', 'Search');
    setDefault(preferences, 'isSearching', {ISI: false, NYU: false});
    setDefault(preferences, 'searchTimeout', undefined);

    setDefault(preferences, 'error', {ISI: undefined, NYU: undefined});
    setDefault(preferences, 'success', {ISI: undefined, NYU: undefined});

    // Set ISI or NYU
    setDefault(preferences, 'sourceMode', 'ISI');

    setDefault(preferences, 'leftJoinVariables', new Set());
    setDefault(preferences, 'rightJoinVariables', new Set());

    setDefault(preferences, 'datamartIndexMode', 'Link');

    setDefault(preferences, 'indexLink', ''); // https://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data
    setDefault(preferences, 'indexFileType', 'csv');

    setDefault(preferences, 'indexScrape', ''); // https://www.w3schools.com/html/html_tables.asp

    setDefault(preferences, 'joinPairs', []);

    setDefault(preferences, 'exactMatch', true);
};
