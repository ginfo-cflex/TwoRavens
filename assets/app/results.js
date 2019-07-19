import m from 'mithril';

import * as app from "./app";
import * as plots from "./plots";

import * as solverRook from './solvers/rook';
import * as solverD3M from './solvers/d3m';

import * as common from "./../common/common";
import Table from "./../common/views/Table";
import Dropdown from "./../common/views/Dropdown";
import Panel from "../common/views/Panel";
import Subpanel from "../common/views/Subpanel";
import MenuHeaders from "../common/views/MenuHeaders";
import Button from "../common/views/Button";
import Icon from "../common/views/Icon";
import MenuTabbed from "../common/views/MenuTabbed";

import {bold, italicize, preformatted} from "./index";
import PlotVegaLite from "./views/PlotVegaLite";
import ConfusionMatrix from "./views/ConfusionMatrix";
import Flowchart from "./views/Flowchart";
import ButtonRadio from "../common/views/ButtonRadio";
import VariableImportance from "./views/VariableImportance";
import ModalVanilla from "../common/views/ModalVanilla";

export let leftpanel = () => {

    let ravenConfig = app.workspace.raven_config;
    let resultsProblem = app.getResultsProblem();

    if (!resultsProblem) return;

    let resultsContent = [
        m('div', {style: {display: 'inline-block', margin: '1em'}},
            m('h4', `${ravenConfig.resultsProblem} for `, m('div[style=display:inline-block]', m(Dropdown, {
                id: 'targetDropdown',
                items: resultsProblem.targets,
                activeItem: resultsPreferences.target,
                onclickChild: value => resultsPreferences.target = value,
                style: {'margin-left': '1em'}
            }))),
            // m(Dropdown, {
            //     id: 'pipelineDropdown',
            //     items: Object.keys(ravenConfig.problems).filter(key =>
            //         Object.keys(ravenConfig.problems[key].solutions)
            //             .reduce((sum, source) => sum + Object.keys(ravenConfig.problems[key].solutions[source]).length, 0)),
            //     activeItem: ravenConfig.resultsProblem,
            //     onclickChild: app.setResultsProblem
            // })
        ),
        m('div#modelComparisonOption', {style: {display: 'inline-block'}},
            m('input#modelComparisonCheck[type=checkbox]', {
                onclick: m.withAttr("checked", setModelComparison),
                checked: modelComparison,
                style: {margin: '.25em'}
            }),
            m('label#modelComparisonLabel', {
                title: 'select multiple models to compare',
                style: {display: 'inline-block'}
            }, 'Model Comparison')
        ),
        m(MenuHeaders, {
            id: 'pipelineMenu',
            sections: [
                {
                    idSuffix: 'DiscoveredSolutions',
                    value: [
                        m('[style=display:inline-block;margin-right:1em]', 'Discovered Solutions'),
                        resultsProblem.d3mSolverState !== undefined && [
                            common.loaderSmall('D3M'),
                            m('div[style=font-size:medium;margin-left:1em;display:inline-block]', resultsProblem.d3mSolverState)
                        ]
                    ],
                    contents: m(Table, {
                        id: 'pipelineTable',
                        data: Object.values(resultsProblem.solutions.d3m)
                            .map(solution => getSolutionAdapter(resultsProblem, solution))
                            .map(adapter => Object.assign({
                                    ID: String(adapter.getName()), Solution: adapter.getModel()
                                },
                                [resultsProblem.metric, ...resultsProblem.metrics]
                                    .reduce((out, metric) => Object.assign(out, {
                                        [metric]: app.formatPrecision(adapter.getScore(metric))
                                    }), {}))),
                        sortable: true,
                        sortHeader: selectedMetric.d3m,
                        setSortHeader: header => selectedMetric.d3m = header,
                        sortDescending: !reverseSet.includes(selectedMetric.d3m),
                        activeRow: new Set(resultsProblem.selectedSolutions.d3m),
                        onclick: pipelineId => setSelectedSolution(resultsProblem, 'd3m', pipelineId)
                    })
                },
                app.callSolverEnabled && {
                    idSuffix: 'BaselineSolutions',
                    value: [
                        m('[style=display:inline-block;margin-right:1em]', 'Baselines'),
                        // app.workspace.raven_config.rook === resultsProblem && common.loaderSmall('Rook')
                    ],
                    contents: [
                        // m(Subpanel, {
                        //     id: 'addModelSubpanel',
                        //     onclick: app.setResultsProblem
                        // }),

                        m(Table, {
                            id: 'pipelineTable',
                            headers: ['Solution', 'Score'],
                            data: Object.keys(resultsProblem.solutions.rook)
                                .map(solutionId => [
                                    solutionId,
                                    solverRook.getScore(resultsProblem, resultsProblem.solutions.rook[solutionId])
                                ]),
                            sortHeader: 'Score',
                            sortFunction: sortPipelineTable,
                            activeRow: new Set(resultsProblem.selectedSolutions.rook),
                            onclick: pipelineId => setSelectedSolution(resultsProblem, 'rook', pipelineId),
                            tableTags: m('colgroup',
                                m('col', {span: 1}),
                                m('col', {span: 1, width: '30%'}))
                        })
                    ]
                }
            ]
        })
    ];

    let tabbedResults = m(MenuTabbed, {
        id: 'resultsMenu',
        attrsAll: {style: {height: 'calc(100% - 8px)'}},
        currentTab: leftTabResults,
        callback: setLeftTabResults,
        sections: [
            {
                value: 'Problems',
                contents: m(Table, {
                    data: Object.keys(app.workspace.raven_config.problems)
                        .filter(problemId => 'd3mSearchId' in app.workspace.raven_config.problems[problemId])
                        .map(problemId => app.workspace.raven_config.problems[problemId])
                        .map(problem => [
                            problem.problemID,
                            problem.targets.join(', '),
                            problem.d3mSearchId,
                            m(Button, {
                                title: 'stop the search',
                                class: 'btn-sm',
                                onclick: () => solverD3M.stopSearch(problem.d3mSearchId)
                            }, m(Icon, {name: 'stop'}))
                        ]),
                    headers: ['Name', 'Targets', 'Search ID', 'Stop'],
                    activeRow: app.workspace.raven_config.resultsProblem,
                    onclick: app.setResultsProblem
                })
            },
            {
                value: 'Solutions',
                contents: resultsContent
            }
        ]
    });

    return m(Panel, {
            side: 'left',
            label: 'Results',
            hover: window.innerWidth < 1000,
            width: '600px'
        },
        // there seems to be a strange mithril bug here - when returning back to model from results,
        // the dom element for MenuTabbed is reused, but the state is incorrectly transitioned, leaving an invalid '[' key.
        // "Fixed" by wrapping in a div, to prevent the dom reuse optimization
        m('div', {style: {height: 'calc(100% - 50px)', overflow: 'auto'}},
            tabbedResults))
};

export class CanvasSolutions {

    oninit() {
        this.confusionFactor = undefined;
        this.confusionMode = 'Stack';
        app.updateRightPanelWidth()
    }

    predictionSummary(problem, adapters) {

        if (problem.task.toLowerCase().includes('regression') || problem.task.toLowerCase() === 'timeseriesforecasting') {
            let summaries = adapters.map(adapter => ({
                name: adapter.getName(),
                fittedValues: adapter.getFittedValues(resultsPreferences.target),
                actualValues: adapter.getActualValues(resultsPreferences.target)
            })).filter(summary => summary.fittedValues && summary.actualValues);

            if (summaries.length === 0) return common.loader('PredictionSummary');

            let xData = summaries.reduce((out, summary) =>
                Object.assign(out, {[summary.name]: summary.fittedValues}), {});
            let yData = summaries.reduce((out, summary) =>
                Object.assign(out, {[summary.name]: summary.actualValues}), {});

            let xName = 'Fitted Values';
            let yName = 'Actual Values';
            let title = 'Fitted vs. Actuals for predicting ' + problem.targets.join(', ');
            let legendName = 'Solution Name';

            return m('div', {
                style: {'height': '500px'}
            }, m(PlotVegaLite, {
                specification: plots.vegaScatter(xData, yData, xName, yName, title, legendName),
            }))
        }

        if (problem.task.toLowerCase().includes('classification')) {

            let summaries = adapters.map(adapter => ({
                name: adapter.getName(),
                confusionMatrix: adapter.getConfusionMatrix(resultsPreferences.target)
            })).filter(summary => summary.confusionMatrix);
            if (summaries.length === 0) return common.loader('PredictionSummary');

            let setConfusionFactor = factor => this.confusionFactor = factor === 'undefined' ? undefined : factor;

            // ignore summaries without confusion matrices
            summaries = summaries.filter(summary => summary.confusionMatrix);
            if (summaries.length === 0) return;

            // collect classes from all summaries
            let classes = [...new Set(summaries.flatMap(summary => summary.confusionMatrix.classes))]

            // convert to 2x2 if factor is set
            if (this.confusionFactor !== undefined)
                summaries.forEach(summary => summary.confusionMatrix = confusionMatrixFactor(
                    summary.confusionMatrix.data,
                    summary.confusionMatrix.classes,
                    this.confusionFactor));

            // prevent invalid confusion matrix selection
            if (this.confusionMatrixSolution === undefined || !summaries.find(summary => summary.name === this.confusionMatrixSolution))
                this.confusionMatrixSolution = summaries[0].name;

            return [
                m('div[style=margin-bottom:1em]',
                    m('label#confusionFactorLabel', 'Confusion Matrix Factor: '),
                    m('[style=display:inline-block]', m(Dropdown, {
                        id: 'confusionFactorDropdown',
                        items: ['undefined', ...classes],
                        activeItem: this.confusionFactor,
                        onclickChild: setConfusionFactor,
                        style: {'margin-left': '1em'}
                    }))),
                summaries.length > 1 && m('div',
                    m('label', 'Confusion Matrix Mode:'),
                    m(ButtonRadio, {
                        id: 'confusionModeButtonBar',
                        onclick: mode => this.confusionMode = mode,
                        activeSection: this.confusionMode,
                        sections: [
                            {value: 'Stack', title: 'render confusion matrices in the same space'},
                            {value: 'List', title: 'render confusion matrices above/below each other'}
                        ],
                        attrsAll: {style: {'margin-left': '1em', width: '10em', display: 'inline-block'}},
                    })),
                m('label', 'Pipeline:'),
                m({Stack: MenuTabbed, List: MenuHeaders}[this.confusionMode], {
                    id: 'confusionMenu',
                    currentTab: this.confusionMatrixSolution,
                    callback: solutionId => this.confusionMatrixSolution = solutionId,
                    sections: summaries.map(summary => ({
                        value: summary.name,
                        contents: [
                            summary.confusionMatrix.classes.length === 2 && m(Table, {
                                id: 'resultsPerformanceTable',
                                headers: ['metric', 'score'],
                                data: generatePerformanceData(summary.confusionMatrix.data),
                                attrsAll: {style: {width: 'calc(100% - 2em)', margin: '1em'}}
                            }),
                            summary.confusionMatrix.data.length < 100 ? summary.confusionMatrix.classes.length > 0 ? m('div', {
                                style: {'min-height': '500px', 'min-width': '500px'}
                            }, m(ConfusionMatrix, Object.assign({}, summary.confusionMatrix, {
                                id: 'resultsConfusionMatrixContainer' + summary.name,
                                title: `Confusion Matrix for ${problem.targets[0]}`,
                                startColor: '#ffffff', endColor: '#e67e22',
                                margin: {left: 10, right: 10, top: 50, bottom: 10},
                                attrsAll: {style: {height: '600px'}}
                            })))
                                : 'Too few classes for confusion matrix! There is a data mismatch.'
                                : 'Too many classes for confusion matrix!'
                        ]
                    }))
                })
            ]
        }
    };

    scoresSummary(problem, adapters) {

        if (resultsPreferences.plotScores === 'all')
            adapters = getSolutions(problem).map(solution => getSolutionAdapter(problem, solution));

        return [
            m('div', m('[style=display:inline-block]', 'Graph'), m(ButtonRadio, {
                id: 'plotScoresButtonBar',
                onclick: mode => resultsPreferences.plotScores = mode,
                activeSection: resultsPreferences.plotScores,
                sections: [{value: 'all'}, {value: 'selected'}],
                attrsAll: {style: {'margin': '0 .5em', display: 'inline-block', width: 'auto'}},
                attrsButtons: {class: 'btn-sm', style: {width: 'auto'}},
            }), m('[style=display:inline-block]', 'solutions.')),
            [problem.metric, ...problem.metrics].map(metric => m(PlotVegaLite, {
                specification: {
                    "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
                    "description": `${metric} scores for ${problem.problemID}.`,
                    data: {values: adapters.map(adapter => ({
                            ID: adapter.getName(),
                            [metric]: adapter.getScore(metric)
                        })).filter(point => point[metric] !== undefined)},
                    "mark": "bar",
                    "encoding": {
                        "x": {"field": 'ID', "type": "nominal"},
                        "y": {
                            "field": metric, "type": "quantitative",
                            scale: (metric in app.d3mMetricDomains)
                                ? {domain: app.d3mMetricDomains[metric]}
                                : {}
                        },
                        "tooltip": [
                            {"field": 'ID', "type": "nominal"},
                            {"field": metric, "type": "quantitative"}
                        ]
                    }
                }
            }))
        ]
    }

    variableImportance(problem, adapter) {

        let importanceContent = common.loader('VariableImportance');

        if (resultsPreferences.mode === 'PDP') {
            importanceContent = [
                m('label', 'Importance for predictor:'),
                m(Dropdown, {
                    id: 'predictorImportanceDropdown',
                    items: problem.predictors,
                    onclickChild: mode => resultsPreferences.predictor = mode,
                    activeItem: resultsPreferences.predictor,
                })
            ];

            let importanceData = ({
                EFD: adapter.getImportanceEFD,
                Partials: adapter.getImportancePartials
            })[resultsPreferences.mode](resultsPreferences.predictor);

            if (importanceData) importanceContent.push(m(VariableImportance, {
                mode: resultsPreferences.mode,
                data: importanceData,
                problem: problem,
                predictor: resultsPreferences.predictor,
                target: resultsPreferences.target,
                yLabel: valueLabel,
                variableLabel: variableLabel
            }));
            else importanceContent.push(common.loader('VariableImportance'))
        }
        else {
            let importanceData = problem.predictors.reduce((out, predictor) => Object.assign(out, {[predictor]: ({
                EFD: adapter.getImportanceEFD,
                Partials: adapter.getImportancePartials
            })[resultsPreferences.mode](predictor)}), {});

            // reassign content if some data is not undefined
            let importancePlots = Object.keys(importanceData).map(predictor => importanceData[predictor] && [
                bold(predictor),
                m(VariableImportance, {
                    mode: resultsPreferences.mode,
                    data: importanceData[predictor],
                    problem: problem,
                    predictor,
                    target: resultsPreferences.target,
                    yLabel: valueLabel,
                    variableLabel: variableLabel
                })
            ]).filter(_ => _);

            if (importancePlots.length > 0) importanceContent = [
                m('div[style=margin: 1em]', italicize("Empirical first differences"), ` is a tool to measure variable importance from the empirical distribution of the data. The "${valueLabel}" axis refers to the frequency of the dependent variable as the predictor (x) varies along its domain. Parts of the domain where the fitted and actual values align indicate high utility from the predictor.`),
                importancePlots
            ];
        }
        return [
            m('label', 'Variable importance mode:'),
            m(ButtonRadio, {
                id: 'modeImportanceButtonBar',
                onclick: mode => resultsPreferences.mode = mode,
                activeSection: resultsPreferences.mode,
                sections: [
                    {value: 'EFD', title: 'empirical first differences'},
                    {value: 'Partials', title: 'model prediction as predictor varies over its domain'}
                ]
            }),
            importanceContent
        ]
    }

    visualizePipeline(solution) {

        // only called if the pipeline flowchart is rendered
        let pipelineFlowchartPrep = pipeline => {
            let steps = pipeline.steps.map((pipeStep, i) => ({
                key: 'Step ' + i,
                color: common.grayColor,
                // special coloring is not enabled for now
                // color: {
                //     'data': common.grayColor,
                //     'byudml': common.dvColor,
                //     'sklearn_wrap': common.csColor
                // }[pipeStep.primitive.python_path.split('.')[2]] || common.grayColor,
                // the table is overkill, but we could certainly add more info here
                summary: m(Table, {
                    id: 'pipelineFlowchartSummary' + i,
                    // abbreviation: 40,
                    data: {
                        'Name': pipeStep['primitive']['primitive'].name,
                        'Method': pipeStep['primitive']['primitive']['pythonPath'].replace('d3m.primitives.', '')
                    },
                    attrsAll: {style: {'margin-bottom': 0, padding: '1em'}}
                }),
                content: m(Table, {
                    id: 'pipelineTableStep' + i,
                    abbreviation: 40,
                    data: pipeStep,
                    nest: true
                })
            }));

            let inputs = 'inputs' in pipeline && m(Table, {
                id: 'pipelineInputsTable',
                data: pipeline.inputs,
                attrsAll: {style: {'margin-bottom': 0, 'padding': '1em'}}
            });
            let outputs = 'outputs' in pipeline && m(Table, {
                id: 'pipelineOutputsTable',
                data: pipeline.outputs,
                attrsAll: {style: {'margin-bottom': 0, 'padding': '1em'}}
            });

            return [
                {color: common.csColor, key: 'Inputs', summary: inputs, content: inputs},
                ...steps,
                {color: common.csColor, key: 'Outputs', summary: outputs, content: outputs}
            ];
        };

        return solution.pipeline && m('div', {
                style: {
                    height: 'calc(100% - 30px)',
                    overflow: 'auto'
                }
            },
            m('div', {style: {'font-weight': 'bold', 'margin': '1em'}}, 'Overview: '),
            m(Table, {
                id: 'pipelineOverviewTable',
                data: Object.keys(solution.pipeline).reduce((out, entry) => {
                    if (!['inputs', 'steps', 'outputs', 'id', 'users', 'digest'].includes(entry))
                        out[entry] = solution.pipeline[entry];
                    return out;
                }, {}),
                attrsAll: {
                    style: {
                        margin: '1em',
                        width: 'calc(100% - 2em)',
                        border: common.borderColor,
                        'box-shadow': '0px 5px 5px rgba(0, 0, 0, .2)'
                    }
                },
                nest: true
            }),
            m('div', {style: {'font-weight': 'bold', 'margin': '1em'}}, 'Steps: '),
            m(Flowchart, {
                labelWidth: '5em',
                steps: pipelineFlowchartPrep(solution.pipeline)
            }))
    };

    view(vnode) {
        let {problem} = vnode.attrs;
        if (!problem) return;

        // ensure valid state of selected predictor, target
        if (!problem.predictors.includes(resultsPreferences.predictor))
            resultsPreferences.predictor = problem.predictors[0];
        if (!problem.targets.includes(resultsPreferences.target))
            resultsPreferences.target = problem.targets[0];

        let problemSummary = m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Problem Description',
            shown: resultsSubpanels['Problem Description'],
            setShown: state => {
               resultsSubpanels['Problem Description'] = state;
               if(state){
                 // behavioral logging
                 let logParams = {
                               feature_id: 'VIEW_PROBLEM_DESCRIPTION',
                               activity_l1: 'MODEL_SELECTION',
                               activity_l2: 'MODEL_EXPLANATION'
                             };
                 app.saveSystemLogEntry(logParams);
               }
            }
        }, m(Table, {
            headers: ['Variable', 'Data'],
            data: [
                ['Dependent Variables', problem.targets],
                ['Predictors', app.getPredictorVariables(problem)],
                ['Description', preformatted(app.getDescription(problem))],
                ['Task', problem.task]
            ]
        }));

        let selectedSolutions = getSelectedSolutions(problem);
        if (selectedSolutions.length === 0)
            return m('div', {style: {margin: '1em 0px'}}, problemSummary);

        let solutionAdapters = selectedSolutions
            .map(solution => getSolutionAdapter(problem, solution));
        let firstAdapter = solutionAdapters[0];
        let firstSolution = selectedSolutions[0];

        let solutionSummary = selectedSolutions.length === 1 && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Solution Description',
            shown: resultsSubpanels['Solution Description'],
            setShown: state => {
              resultsSubpanels['Solution Description'] = state;
              if(state){
                // behavioral logging
                let logParams = {
                              feature_id: 'VIEW_SOLUTION_DESCRIPTION',
                              activity_l1: 'MODEL_SELECTION',
                              activity_l2: 'MODEL_EXPLANATION'
                            };
                app.saveSystemLogEntry(logParams);
              }
            }
        }, m(Table, {
            headers: ['Variable', 'Data'],
            data: [
                ['Source', firstSolution.source]
            ].concat(firstSolution.source === 'rook' ? [
                ['Label', firstSolution.meta.label],
                ['Caret/R Method', firstSolution.meta.method],
                ['Tags', firstSolution.meta.tags]
            ] : firstSolution.source === 'd3m' ? [
                ['Pipeline ID', firstSolution.pipelineId],
                ['Status', firstSolution.status],
                ['Created', new Date(firstSolution.created).toUTCString()]
            ] : [])
        }));

        let predictionSummary = m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Prediction Summary',
            shown: resultsSubpanels['Prediction Summary'],
            setShown: state => {
                resultsSubpanels['Prediction Summary'] = state;
                if (state) {
                    // behavioral logging
                    let logParams = {
                        feature_id: 'VIEW_PREDICTION_SUMMARY',
                        activity_l1: 'MODEL_SELECTION',
                        activity_l2: 'MODEL_EXPLANATION'
                    };
                    app.saveSystemLogEntry(logParams);
                }
            }
        }, resultsSubpanels['Prediction Summary'] && this.predictionSummary(problem, solutionAdapters));

        let scoresSummary = m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Scores Summary',
            shown: resultsSubpanels['Scores Summary'],
            setShown: state => {
                resultsSubpanels['Scores Summary'] = state;
                if (state) {
                    // behavioral logging
                    let logParams = {
                        feature_id: 'VIEW_SCORES_SUMMARY',
                        activity_l1: 'MODEL_SELECTION',
                        activity_l2: 'MODEL_EXPLANATION'
                    };
                    app.saveSystemLogEntry(logParams);
                }
            }
        }, resultsSubpanels['Scores Summary'] && this.scoresSummary(problem, solutionAdapters));

        let variableImportance = firstAdapter && firstAdapter.getSource() === 'd3m' && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Variable Importance',
            shown: resultsSubpanels['Variable Importance'],
            setShown: state => {
                resultsSubpanels['Variable Importance'] = state;
                if(state){
                    // behavioral logging
                    let logParams = {
                        feature_id: 'VIEW_VARIABLE_IMPORTANCE',
                        activity_l1: 'MODEL_SELECTION',
                        activity_l2: 'MODEL_EXPLANATION'
                    };
                    app.saveSystemLogEntry(logParams);
                }
            }
        }, resultsSubpanels['Variable Importance'] && this.variableImportance(problem, firstAdapter));

        let visualizePipelinePanel = selectedSolutions.length === 1 && firstSolution.source === 'd3m' && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Visualize Pipeline',
            shown: resultsSubpanels['Visualize Pipeline'],
            setShown: state => {
              resultsSubpanels['Visualize Pipeline'] = state;
              if(state){
                // behavioral logging
                let logParams = {
                              feature_id: 'VIEW_VISUALIZE_PIPELINE',
                              activity_l1: 'MODEL_SELECTION',
                              activity_l2: 'MODEL_EXPLANATION'
                            };
                app.saveSystemLogEntry(logParams);
              }
            }
        }, resultsSubpanels['Visualize Pipeline'] && this.visualizePipeline(firstSolution));

        let performanceStatsContents = firstSolution.source === 'rook' && Object.keys(firstSolution.models)
            .filter(target => firstSolution.models[target].statistics)
            .map(target => m('div',
                m('h5', target),
                m(Table, {
                    data: firstSolution.models[target].statistics[0]
                })));
        let performanceStats = performanceStatsContents.length > 0 && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Performance Statistics',
            shown: resultsSubpanels['Performance Statistics'],
            setShown: state => resultsSubpanels['Performance Statistics'] = state
        }, performanceStatsContents);

        let coefficientsContents = firstSolution.source === 'rook' && Object.keys(firstSolution.models)
            .filter(target => firstSolution.models[target].coefficients !== undefined)
            .map(target => m('div',
                m('h5', target),
                m(Table, {data: ['intercept', ...app.getPredictorVariables(problem)].map((predictor, i) => [
                        predictor,
                        firstSolution.models[target].coefficients[i]
                    ])}),
                firstSolution.models[target].coefficientCovarianceMatrix && m(ConfusionMatrix, {
                    id: target + 'CovarianceMatrix',
                    title: 'Coefficient Covariance Matrix for ' + target,
                    data: firstSolution.models[target].coefficientCovarianceMatrix,
                    startColor: '#e9ede8',
                    endColor: '#5770b0',
                    xLabel: '',
                    yLabel: '',
                    classes: ['intercept', ...app.getPredictorVariables(problem)],
                    margin: {left: 10, right: 10, top: 50, bottom: 10},
                    attrsAll: {style: {height: '600px'}}
                })));
        let coefficientMatrix = coefficientsContents.length > 0 && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Coefficients',
            shown: resultsSubpanels['Coefficients'],
            setShown: state => resultsSubpanels['Coefficients'] = state
        }, coefficientsContents);


        let prepareANOVA = table => [...app.getPredictorVariables(problem), 'Residuals']
            .map(predictor => table.find(row => row._row === predictor))
            .map(row => ({
                'Predictor': row._row,
                'Sum Sq': row['Sum Sq'],
                'Df': row.Df,
                'Mean Sq': row['Mean Sq'],
                'F value': row['F value'],
                'P-value': row['Pr(>F)']
            }));

        let anovaTablesContent = firstSolution.source === 'rook' && Object.keys(firstSolution.models)
            .filter(target => firstSolution.models[target].anova)
            .map(target => m('div',
                m('h5', target),
                m(Table, {data: prepareANOVA(firstSolution.models[target].anova)})));
        let anovaTables = (anovaTablesContent || []).length > 0 && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'ANOVA Tables',
            shown: resultsSubpanels['ANOVA Tables'],
            setShown: state => resultsSubpanels['ANOVA Tables'] = state
        }, anovaTablesContent);

        let VIFContents = firstSolution.source === 'rook' && Object.keys(firstSolution.models)
            .filter((target, i) => i === 0 && firstSolution.models[target].vif)
            .map(target => m('div',
                m(Table, {
                    data: Object.keys(firstSolution.models[target].vif).map(predictor => [
                        predictor,
                        firstSolution.models[target].vif[predictor][0]
                    ])
                })));
        let VIF = (VIFContents || []).length === 1 && m(Subpanel, {
            style: {margin: '0px 1em'},
            header: 'Variance Inflation',
            shown: resultsSubpanels['Variance Inflation'],
            setShown: state => resultsSubpanels['Variance Inflation'] = state
        }, VIFContents);

        return m('div', {style: {margin: '1em 0px'}},
            problemSummary,
            solutionSummary,
            predictionSummary,
            scoresSummary,
            variableImportance,
            visualizePipelinePanel,
            performanceStats,
            coefficientMatrix,
            anovaTables,
            VIF
        );
    }
}

let getSolutionAdapter = (problem, solution) => ({
    rook: solverRook.getSolutionAdapter, d3m: solverD3M.getSolutionAdapter
}[solution.source](problem, solution));

let leftTabResults = 'Solutions';
let setLeftTabResults = tab => leftTabResults = tab;

let resultsPreferences = {
    mode: 'EFD',
    predictor: undefined,
    target: undefined,
    plotScores: 'all'
};

// labels for variable importance X/Y axes
export let valueLabel = "Observation";
export let variableLabel = "Dependent Variable";

export let selectedMetric = {
    d3m: undefined,
    rook: undefined
};

// array of metrics to sort low to high
export let reverseSet = [
    "meanSquaredError", "rootMeanSquaredError", "meanAbsoluteError", "hammingLoss", "rank", "loss"
];

/**
 Sort the Pipeline table, putting the best score at the top
 */
let sortPipelineTable = (a, b) => typeof a === 'string'
    ? app.omniSort(a, b)
    : (b - a) * (reverseSet.includes(selectedMetric.d3m) ? -1 : 1);

let resultsSubpanels = {
    'Prediction Summary': true,
    'Scores Summary': false,
    'Variance Inflation': false,
    'ANOVA Tables': false,
    'Coefficients': false,
    'Performance Statistics': false,
    'Visualize Pipeline': false,
    'Solution Description': false,
    'Problem Description': false,
    'Variable Importance': false
};


// when selected, the key/value [mode]: [pipelineID] is set.
export let setSelectedSolution = (problem, source, solutionId) => {
    solutionId = String(solutionId);

    // set behavioral logging
    let logParams = {
        activity_l1: 'MODEL_SELECTION',
        other: {solutionId: solutionId}
    };

    if (!problem) return;
    if (!(source in problem.selectedSolutions)) problem.selectedSolutions[source] = [];

    if (modelComparison) {

        problem.selectedSolutions[source].includes(solutionId)
            ? app.remove(problem.selectedSolutions[source], solutionId)
            : problem.selectedSolutions[source].push(solutionId);

        // set behavioral logging
        logParams.feature_id = 'RESULTS_COMPARE_SOLUTIONS';
        logParams.activity_l2 = 'MODEL_COMPARISON';
    } else {
        problem.selectedSolutions = Object.keys(problem.selectedSolutions)
            .reduce((out, source) => Object.assign(out, {[source]: []}, {}), {});
        problem.selectedSolutions[source] = [solutionId];

        // set behavioral logging
        logParams.feature_id = 'RESULTS_SELECT_SOLUTION';
        logParams.activity_l2 = 'MODEL_SUMMARIZATION';
    }

    // record behavioral logging
    app.saveSystemLogEntry(logParams);

};

export let getSolutions = (problem, source) => {
    if (!problem) return [];

    if (source) {
        if (!(source in problem.solutions)) problem.solutions[source] = [];
        Object.values(problem.solutions[source]);
    }

    return Object.values(problem.solutions)
        .flatMap(source => Object.values(source))
};

export let getSelectedSolutions = (problem, source) => {
    if (!problem) return [];

    if (!source) return Object.keys(problem.selectedSolutions)
        .flatMap(source => problem.selectedSolutions[source]
            .map(id => problem.solutions[source][id])).filter(_=>_);

    problem.selectedSolutions[source] = problem.selectedSolutions[source] || [];
    return problem.selectedSolutions[source]
        .map(id => problem.solutions[source][id]).filter(_=>_)
};

// When enabled, multiple pipelineTable pipelineIDs may be selected at once
export let modelComparison = false;
export let setModelComparison = state => {
    let resultsProblem = app.getResultsProblem();
    let selectedSolutions = getSelectedSolutions(resultsProblem);

    modelComparison = state;
    if (selectedSolutions.length > 1 && !modelComparison)
        setSelectedSolution(resultsProblem, selectedSolutions[0].source, selectedSolutions[0]);

};

export let confusionMatrixFactor = (matrix, labels, factor) => {
    let collapsed = [[0, 0], [0, 0]];
    labels.forEach((xLabel, x) => labels.forEach((yLabel, y) =>
        collapsed[xLabel === factor ? 0 : 1][yLabel === factor ? 0 : 1] += matrix[x][y]
    ));

    return {
        data: collapsed,
        classes: [factor, 'not ' + factor]
    }
};

// generate an object containing accuracy, recall, precision, F1, given a 2x2 confusion data matrix
// the positive class is the upper left block
export function generatePerformanceData(confusionData2x2) {

    let tp = confusionData2x2[0][0];
    let fn = confusionData2x2[0][1];
    let fp = confusionData2x2[1][0];
    let tn = confusionData2x2[1][1];

    let p = tp + fn;
    let n = fp + tn;

    let round = (number, digits) => Math.round(number * Math.pow(10, digits)) / Math.pow(10, digits)

    return {
        f1: round(2 * tp / (2 * tp + fp + fn), 2),
        precision: round(tp / (tp + fp), 2), // positive predictive value
        recall: round(tp / (tp + fn), 2), // sensitivity, true positive rate
        accuracy: round((tp + tn) / (p + n), 2),

        // specificity: round(fp / (fp + tn), 2),
        // 'true positive rate': round(tp / (tp + fn), 2), // already included with recall
        // 'true negative rate': round(tn / (tn + fp), 2),
        // 'false positive rate': round(fp / (fp + tn), 2),
        // 'false negative rate': round(fn / (fn + tp), 2), // miss rate
    }
}

export let showFinalPipelineModal = false;
export let setShowFinalPipelineModal = state => showFinalPipelineModal = state;

export let finalPipelineModal = () => {
    let resultsProblem = app.getResultsProblem();

    let chosenSolution = getSolutions(resultsProblem, 'd3m').find(solution => solution.chosen);
    if (!chosenSolution) return;

    let adapter = solverD3M.getSolutionAdapter(resultsProblem, chosenSolution);

    return m(ModalVanilla, {
            id: 'finalPipelineModal',
            setDisplay: setShowFinalPipelineModal
        },
        m('h4', 'Pipeline ', adapter.getName()),
        'Task Two Complete. Your selected pipeline has been submitted.'

        // * lots of room for cool activities *

        // m(Table, {
        //     id: 'finalPipelineTable',
        //     data: []
        // })
    )
};
