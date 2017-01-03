from itertools import combinations, chain

import numpy as np
from scipy import stats
from scipy.optimize import minimize

from pgmpy.estimators import BaseEstimator
from pgmpy.factors import TabularCPD, Factor, factor_product
from pgmpy.models import BayesianModel, MarkovModel


class MaximumLikelihoodEstimator(BaseEstimator):
    """
    Class used to compute parameters for a model using Maximum Likelihood Estimate.

    Parameters
    ----------
    model: pgmpy.models.BayesianModel or pgmpy.models.MarkovModel or pgmpy.models.NoisyOrModel
        model for which parameter estimation is to be done

    data: pandas DataFrame object
        datafame object with column names same as the variable names of the network

    Examples
    --------
    >>> import numpy as np
    >>> import pandas as pd
    >>> from pgmpy.models import BayesianModel
    >>> from pgmpy.estimators import MaximumLikelihoodEstimator
    >>> values = pd.DataFrame(np.random.randint(low=0, high=2, size=(1000, 5)),
    ...                       columns=['A', 'B', 'C', 'D', 'E'])
    >>> model = BayesianModel([('A', 'B'), ('C', 'B'), ('C', 'D'), ('B', 'E')])
    >>> estimator = MaximumLikelihoodEstimator(model, values)
    """
    def __init__(self, model, data):
        # if not isinstance(model, BayesianModel):
        #     raise NotImplementedError("Maximum Likelihood Estimate is only implemented of BayesianModel")

        super().__init__(model, data)

    def get_parameters(self, **kwargs):
        """
        Method used to get parameters.

        Returns
        -------
        parameters: list
            List containing all the parameters. For Bayesian Model it would be list of CPDs'
            for Markov Model it would be a list of factors

        Examples
        --------
        >>> import numpy as np
        >>> import pandas as pd
        >>> from pgmpy.models import BayesianModel
        >>> from pgmpy.estimators import MaximumLikelihoodEstimator
        >>> values = pd.DataFrame(np.random.randint(low=0, high=2, size=(1000, 5)),
        ...                       columns=['A', 'B', 'C', 'D', 'E'])
        >>> model = BayesianModel([('A', 'B'), ('C', 'B'), ('C', 'D'), ('B', 'E')])
        >>> estimator = MaximumLikelihoodEstimator(model, values)
        >>> estimator.get_parameters()
        """
        if isinstance(self.model, BayesianModel):
            parameters = []

            for node in self.model.nodes():
                parents = self.model.get_parents(node)
                if not parents:
                    state_counts = self.data.ix[:, node].value_counts()
                    cpd = TabularCPD(node, self.node_card[node],
                                     state_counts.values[:, np.newaxis])
                    cpd.normalize()
                    parameters.append(cpd)
                else:
                    parent_card = np.array([self.node_card[parent] for parent in parents])
                    var_card = self.node_card[node]
                    state_counts = self.data.groupby([node] + self.model.predecessors(node)).size()
                    values = state_counts.values.reshape(var_card, np.product(parent_card))
                    cpd = TabularCPD(node, var_card, values,
                                     evidence=parents,
                                     evidence_card=parent_card.astype('int'))
                    cpd.normalize()
                    parameters.append(cpd)

            return parameters

        elif isinstance(self.model, MarkovModel):
            edges = self.model.edges()
            no_of_params = [self.node_card[u] * self.node_card[v] for u, v in edges]
            constants = []
            for u, v in edges:
                value_counts = self.data.groupby([u, v]).size()
                constants.extend(value_counts.values)
            total_params = sum(no_of_params)
            constants = np.array(constants)

            no_of_params.insert(0, 0)
            param_cumsum = np.cumsum(no_of_params)

            def optimize_fun(params):
                factors = []
                for index in range(len(edges)):
                    u, v = edges[index][0], edges[index][1]
                    factors.append(Factor([u, v], [self.node_card[u], self.node_card[v]],
                                          params[param_cumsum[index]: param_cumsum[index + 1]]))
                Z = sum(factor_product(*factors).values)
                return Z - sum(constants * params)

            mini = minimize(optimize_fun, x0=[1]*total_params)
            final_params = mini.x
            score = mini.fun

            factors = []
            for index in range(len(edges)):
                u, v = edges[index][0], edges[index][1]
                factors.append(Factor([u, v], [self.node_card[u], self.node_card[v]],
                                      final_params[param_cumsum[index]: param_cumsum[index + 1]]))

            if 'score' in kwargs and kwargs['score']:
                return factors, score
            else:
                return factors

    def get_model(self, threshold=0.95):
        if isinstance(self.model, BayesianModel):
            nodes = self.data.columns
            self.model.add_nodes_from(nodes)
            edges = []
            for u, v in combinations(nodes, 2):
                f_exp = self.data.groupby([u, v]).size().values
                u_f_obs = self.data.ix[:, u].value_counts().values
                v_f_obs = self.data.ix[:, v].value_counts().values
                if stats.chisquare(f_obs=[i * j for i in u_f_obs for j in v_f_obs], f_exp=f_exp).pvalue < threshold:
                    edges.append((u, v))

            self.model.add_edges_from(edges)
            return nodes, edges

        elif isinstance(self.model, MarkovModel):
            nodes = self.data.columns
            for node in nodes:
                self.node_card[node] = self.data.ix[:, node].unique().size
            self.model.add_nodes_from(nodes)
            all_possible_edges = list(combinations(nodes, 2))
            optimum = 1000000
            optimal_edges = None
            for edge_comb in chain(*[combinations(all_possible_edges, r) for r in range(1, len(all_possible_edges) + 1)]):
                self.model = MarkovModel(edge_comb)
                factors, score = self.get_parameters(score=True)
                if score < optimum:
                    optimum = score
                    optimal_edges = edge_comb

            return nodes, edge_comb
            # check with optimization function and choose the structure which gives
            # the best results.
