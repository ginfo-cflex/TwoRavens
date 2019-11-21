from tworaven_apps.solver_interfaces.util_search import (
    SearchAutoSklearn,
    SearchCaret,
    SearchH2O,
    SearchTPOT,
    SearchLudwig,
    SearchMLJarSupervised,
    SearchMLBox
)


class Solve(object):
    def __init__(self, system, specification, callback_found, system_params=None, search_id=None):
        self.system = system
        self.specification = specification
        self.system_params = system_params or {}
        self.search = {
            'auto_sklearn': SearchAutoSklearn,
            'h2o': SearchH2O,
            'tpot': SearchTPOT,
            'caret': SearchCaret,
            'ludwig': SearchLudwig,
            'mljar-supervised': SearchMLJarSupervised,
            'mlbox': SearchMLBox
        }[system](
            self.specification['search'],
            self.system_params,
            callback_found,
            search_id=search_id)

    def run(self):
        return self.search.run()
