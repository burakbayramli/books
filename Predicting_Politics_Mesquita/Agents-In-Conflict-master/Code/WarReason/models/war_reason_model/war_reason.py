'''
Agent-based model based on War and Reason, BDM & Lalman, 1992.

The model consists of agents characterized by Capabilities and a risk acceptance
coefficient. Agents are connected via a network of alliances, which determines 
their utility of challenging one another, as well as who will come to their aid.

'''
import random
import numpy as np
from scipy.stats import kendalltau
import pandas as pd
import networkx as nx

from consul.meta_model import MetaModel

from consul.model_types.utilities.game_tree import Tree, TreeNode
from consul.model_types.extensive_form import ExtensiveGameModel
from consul.model_types.extensive_form import ExtensiveGameAgent

from consul.agent_types.spe_agent import SPEAgent


class WarAgent(ExtensiveGameAgent):
    '''
    Base class.
    '''

    def __init__(self, capabilities, R, *args, **kwargs):
        '''
        New WarAgent
        '''
        self.capabilities = capabilities
        self.risk = (1-(R/3))/(1+(R/3))
        self.p_win = 0
        self.k_ab = 0
        self.total_utility = 0
        super().__init__(*args, **kwargs)


    def start_conflict(self, world, rival):
        '''
        Called at the beginning of a sub-model run to compute the utility and
        percieved probability of winning for the agent.
        '''

        # Convert network
        adj_mat = nx.to_numpy_matrix(world.network)
        # Build position-to-actor dictionaries
        pos_to_name = {}
        name_to_pos = {}
        for i, node in enumerate(world.network.nodes()):
            pos_to_name[i] = node
            name_to_pos[node] = i

        def get_tau(ccode1, ccode2):
            '''
            Closure to find K-Tau between two actors.
            '''
            x = np.array(adj_mat[name_to_pos[ccode1]])[0]
            y = np.array(adj_mat[name_to_pos[ccode2]])[0]
            return kendalltau(x, y)[0]

        # Find K-tau, similarity between alliance portfolios of self and rival
        k_ab = get_tau(self.name, rival.name)

        # Estimate probability of victory
        top = 0
        bottom = 0
        for actor in world.agents:
            k_ka = get_tau(self.name, actor.name)
            k_kb = get_tau(rival.name, actor.name)
            p = (k_ka - k_kb) / 2
            p *= np.exp(self.risk * (k_ka - k_kb))
            contribution = actor.capabilities * p
            if k_ka > k_kb:
                top += contribution
            bottom += abs(contribution)

        p_win = top / bottom

        # Set the variables at the agent level for utility calculations
        self.k_ab = k_ab
        self.p_win = p_win



class WarModel(ExtensiveGameModel):
    '''
    A single, two-player conflict game as described in BDM & Lalman, 1992.

    '''
    tree_file = "international_interaction_game.json"

    def __init__(self, agents):
        super().__init__(agents, tree=Tree.load_tree(self.tree_file))
        agent1, agent2 = agents
        
        # Calculate utilities
        # TODO: Mirror-imaging
        self.utilities = {node: {agent1.name: 0, agent2.name: 0}
                          for node in self.tree.get_final_nodes()}
        # Agent 1 utilities
        for agent in agents:
            u_sq = 2 - 4 * (0.5**agent.risk)
            ua_da = 2 - 4*((2 - (1 - agent.k_ab))/4)**agent.risk
            ua_db = 2 - 4*((2 - (agent.k_ab - 1))/4)**agent.risk
            u_acq_i = ua_da # This agent wins
            u_acq_j = ua_db # The other agent wins
            u_nego = agent.p_win * ua_da + (1-agent.p_win)*ua_db
            u_cap_j = ua_da - agent.p_win # The other agent surrenders
            u_cap_i = ua_db - (1 - agent.p_win)
            u_war = agent.p_win * (ua_da - agent.p_win - (1-agent.p_win))
            u_war += (1-agent.p_win) * (ua_db - agent.p_win - (1-agent.p_win))

            self.utilities["StatusQuo"][agent.name] = u_sq
            self.utilities["Negotiate_1"][agent.name] = u_nego
            self.utilities["Negotiate_2"][agent.name] = u_nego
            for label in ["War_A1", "War_B1", "War_A2", "War_B2"]:
                self.utilities[label][agent.name] = u_war
            prefix = "A" if agent is agent1 else "B"
            # The other guy winning
            self.utilities["Capitulate_"+prefix+"1"][agent.name] = u_acq_j
            self.utilities["Capitulate_"+prefix+"2"][agent.name] = u_acq_j
            self.utilities["Acquiesce_"+prefix][agent.name] = u_acq_j
            # This agent winning
            prefix = "B" if prefix=="A" else "A"
            self.utilities["Capitulate_"+prefix+"1"][agent.name] = u_acq_i
            self.utilities["Capitulate_"+prefix+"2"][agent.name] = u_acq_i
            self.utilities["Acquiesce_"+prefix][agent.name] = u_acq_i

    @classmethod
    def conflict_from_eugene(cls, eugene_row, agent_class, agent_args={}, 
                             read_utilities=False):
        '''
        Generate a conflict scenario from a row of EUGene data

        Args:
            eugene_row: A pandas row from EUGene
            agent_class: Class of agent to generate
            agent_args: Dictionary of global arguments to pass to all agents
                        (e.g. learning rate)

        '''
        
        agents = []
        for i in ['1', '2']:
            j = '2' if i == '1' else '1'
            # Create agents
            name = eugene_row['ccode' + i]
            nmc = eugene_row['cap_' + i]
            risk_aversion = eugene_row['riskT' + i]
            new_agent_args = dict(name=name,
                              capabilities=nmc,
                              R=risk_aversion)
            for arg, val in agent_args.items():
                new_agent_args[arg] = val
            a = agent_class(**new_agent_args)
            a.p_win = eugene_row["wrTp"+i+"win"]
            a.k_ab = eugene_row["tau_glob"]
            agents.append(a)
        model = cls(agents)
        if not read_utilities:
            return model
        # Fill in utilities

        util_keys = {"StatusQuo": "vsq",
                     "Negotiate_1": "neg",
                     "Negotiate_2": "neg",
                     "War_A1": "wr1",
                     "War_A2": "wr1",
                     "War_B1": "wr2",
                     "War_B2": "wr2",
                     "Capitulate_A1": "cp1",
                     "Capitulate_A2": "cp1",
                     "Capitulate_B1": "cp2",
                     "Capitulate_B2": "cp2",
                     "Acquiesce_A": "ac1",
                     "Acquiesce_B": "ac2" }

        for c, i in enumerate(['1', '2']):
            j = '2' if i == '1' else '1'
            name = agents[c].name
            prefix = "wrTu" + i # War and Reason U(...) using Tau
            for node, col in util_keys.items():
                model.utilities[node][name] = eugene_row[prefix+col]
        return model




class WarWorld(MetaModel):
    '''
    Basic MetaModel for creating a simulated world following the BDM & Lalman
    conflict decision methodology.
    '''

    model_class = WarModel
    agent_class = WarAgent
    agent_count = 20
    density = 0.2
    agent_args = {}
    network = nx.Graph()

    def __init__(self, agent_class, agent_count, density, agent_args={}, 
                 seed=None):
        '''
        Instantiate a new WarWorld model.

        Args:
            ...
        '''
        self.agent_class = agent_class
        self.agent_count = agent_count
        self.agent_args = agent_args
        self.density = density
        super().__init__(self.model_class, agents_per_model=2, seed=seed)
        self.log = []

    def make_agents(self):
        '''
        Create the agents randomly.
        '''
        self.agents = []
        for i in range(self.agent_count):
            nmc = np.random.lognormal(-2.5, 0.2) # Seems to work.
            risk_aversion = np.random.random() * 2 - 1 # Good enough?
            agent_args = dict(name=i,
                              capabilities=nmc,
                              R=risk_aversion)
            for arg, val in self.agent_args.items():
                agent_args[arg] = val
            a = self.agent_class(**agent_args)
            self.agents.append(a)

        # Create the network
        self.network = nx.erdos_renyi_graph(self.agent_count, self.density)

    def step(self):
        '''
        Pick two agents, and have them play the sub-model to completion.
        '''
        pass

class EUGeneWorld(MetaModel):
    '''
    MetaModel that runs a simulation using agents and utilities from the
    EUGene data.
    '''

    model_class = WarModel
    current_row = None

    def __init__(self, agent_class, eugene_data, agent_args={}, 
                 seed=None, copy_data=False, shuffle_rows=False):
        '''

        '''
        self.agent_class = agent_class
        if not copy_data:
            self.eugene_data = eugene_data
        else:
            self.eugene_data = eugene_data.copy()
        self.agent_args = agent_args
        super().__init__(self.model_class, agents_per_model=2, seed=seed)

    def _shuffle_data(self):
        '''
        Shuffle the order of rows in the EUGene data, preserving year ordering.
        '''
        new_data = []
        for group in self.eugene_data.groupby("year"):
            new_order = list(group[1].index)
            random.shuffle(new_order)
            temp = group[1].ix[new_order]
            new_data.append(temp)
        self.eugene_data = pd.concat(new_data)

    def make_agents(self):
        '''
        Create one agent per CCode appearing in the dataset.
        '''
        self.agents = {}
        all_ccodes = set(list(self.eugene_data.ccode1.unique()) + 
                         list(self.eugene_data.ccode2.unique()))
        for ccode in all_ccodes:
            agent_args = dict(name=ccode,
                              capabilities=-9,
                              R=-9)
            for arg, val in self.agent_args.items():
                agent_args[arg] = val
            a = self.agent_class(**agent_args)
            self.agents[ccode] = a

    def step(self, row, return_result=False, training=False):
        '''
        Given a EUGene row, create a war game for it and run to completion.
        '''
        self.current_row = row
        ccode1 = row['ccode1']
        ccode2 = row['ccode2']

        agent1 = self.agents[ccode1]
        agent2 = self.agents[ccode2]

        # Reset best_moves
        agent1.start_conflict(self, agent2)
        agent2.start_conflict(self, agent1)

        model = self.model_class([agent1, agent2])

        util_keys = {"StatusQuo": "vsq",
                     "Negotiate_1": "neg",
                     "Negotiate_2": "neg",
                     "War_A1": "wr1",
                     "War_A2": "wr1",
                     "War_B1": "wr2",
                     "War_B2": "wr2",
                     "Capitulate_A1": "cp1",
                     "Capitulate_A2": "cp1",
                     "Capitulate_B1": "cp2",
                     "Capitulate_B2": "cp2",
                     "Acquiesce_A": "ac1",
                     "Acquiesce_B": "ac2" }

        for c, i in enumerate(['1', '2']):
            name = model.agents[c].name
            prefix = "wrTu" + i # War and Reason U(...) using Tau
            for node, col in util_keys.items():
                model.utilities[node][name] = row[prefix+col]

        if not training:
            model.run()
            if return_result:
                return model.current_node
            else:
                return model
        else:
            node_keys = {"eqTsq": ["StatusQuo"],
                         "eqTnego": ["Negotiate_1", "Negotiate_2"],
                         "eqTacqa": ["Acquiesce_A"],
                         "eqTacqb": ["Acquiesce_B"],
                         "eqTwara": ["War_A1"],
                         "eqTwarb": ["War_B1"]
                         }
            target_node = random.choice(node_keys[row["Equilibrium"]])
            for agent in model.agents:
                agent.train_to_outcome(model, target_node)

    def run(self, report_every=100000, model_assessors={}):
        '''
        Run a step for each row of the data.

        Args:
            output_each: Print current step once every this-many steps
                        (TODO: or TimeDelta)
            model_assessments: DataCollector-style names and functions to run 
                               on each model object.
        '''

        model_assessments = {name: [] for name in model_assessors}
        count = 0
        for i, row in self.eugene_data.iterrows():
            if report_every is not None and count % report_every == 0:
                print(count)
            m = self.step(row, return_result=False)
            for name, assessor in model_assessors.items():
                r = assessor(m)
                model_assessments[name].append(r)
            count += 1
        for name, data in model_assessments.items():
            self.eugene_data[name] = data




