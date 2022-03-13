'''
Implementation of the 'small' extensive-form Crisis Game, based on the one
described in [Signorino, 1999](http://www.jstor.org/stable/2585396).
'''

import random

from consul.meta_model import MetaModel

from consul.model_types.utilities.game_tree import Tree, TreeNode
from consul.model_types.extensive_form import ExtensiveGameModel
from consul.model_types.extensive_form import ExtensiveGameAgent

from consul.agent_types.spe_agent import SPEAgent

from mesa.datacollection import DataCollector

class CrisisModel(ExtensiveGameModel):
    '''
    Model for a single, two-player crisis game as described in Signorino, 1999.

    Possible outcomes are: StatusQuo, War, or one player's capitulation.

    Utilities are as follows:
        Status Quo: Either 0, or 20 iff both agents belong to the same bloc
        Capitulation: The capitulating agent loses its assets, the other agent
            gains those assets.
        War: The winning agent gains the loser's assets, and the loser loses 
            their assets and military strength. Utility is the expected utility 
            of war, based on the probability of victory / defeat. The 
            probability of winning the war is defined as the agent's share of 
            both agent's combined military strength.

    '''
    def __init__(self, agents):
        super().__init__(agents, tree=self.make_tree())
        
        # Calculate utilities
        ## Helper assignments
        agent1, agent2 = agents        
        SQ = (agent1.bloc == agent2.bloc) * 20
        self.utilities["War1"] = {agent1.name: self.war_util(agent1, agent2),
                                  agent2.name: self.war_util(agent2, agent1)}
        self.utilities["War2"] = {agent1.name: self.war_util(agent1, agent2),
                                  agent2.name: self.war_util(agent2, agent1)}
        self.utilities["Capitulate1"] = {agent1.name: -agent1.assets,
                                        agent2.name: agent1.assets}
        self.utilities["Capitulate2"] = {agent2.name: -agent2.assets,
                                        agent1.name: agent2.assets}
        self.utilities["StatusQuo"] = {agent1.name: SQ, agent2.name: SQ}

    
    @staticmethod
    def war_util(ego, alter):
        '''
        Calculate ego's expected utility of war with alter
        '''
        p = ego.mil_strength / (ego.mil_strength + alter.mil_strength)
        return p * alter.assets + (1-p)*(-ego.assets-ego.mil_strength)

    @staticmethod
    def make_tree(file_path=None):
        '''
        Run once to create the game tree described in Signorino, 1999.

        Args:
            file_path: If provided, write the tree object to this file.

        Returns:
            The game tree object.
        '''
        tree = Tree()
        tree.add_node(TreeNode("StatusQuo"))
        tree.add_node(TreeNode("Capitulate1"))
        tree.add_node(TreeNode("War2"))
        tree.add_node(TreeNode("Capitulate2"))
        tree.add_node(TreeNode("War1"))
        tree.add_node(TreeNode("C", {"Fight": "War2", "NotFight": "Capitulate1"}))
        tree.add_node(TreeNode("A", {"Fight": "C", "NotFight": "StatusQuo"}))
        tree.add_node(TreeNode("B", {"Fight": "War1", "NotFight": "Capitulate2"}))
        tree.add_node(TreeNode("Root", {"Fight": "B", "NotFight": "A"}))
        tree.root_node = "Root"
        if file_path:
            tree.to_file(file_path)
        return tree

    def assess_run(self, return_log=False):
        '''
        Compare log to the subgame-perfect equilibrium for the current agents.

        Args:
            return_log: If True, will return the actual SPE log.
                        Otherwise, will just return the Tversky index 
        '''
        # Clone the model with rational agents and solve it.
        sub_model = self.find_equilibrium()
        spe_log = sub_model.log
        if return_log:
            return spe_log
        else:
            return self.log.tversky_index(spe_log)

    def find_equilibrium(self):
        '''
        Find the subgame-perfect equilibrium for this model instance.

        Returns:
            A clone of this model, with RationalAgents and a current_node at
            the SPE terminal node.
        '''
        
        new_agents = []
        for agent in self.agents:
            new_agent = RationalAgent(agent.assets, agent.mil_strength, 
                                      agent.bloc, agent.name)
            new_agents.append(new_agent)
        sub_model = self.__class__(new_agents)
        while sub_model.running:
            sub_model.step()

        return sub_model



class CrisisAgent(ExtensiveGameAgent):
    '''
    The agent prototype to go with the Crisis Model.

    All agents must have the following attributes:
        assets: Value (in utils) of assets being defended
        mil_strength: Military strength which can be brought to bear 
        bloc: Dummy variable; used for determining status-quo utility.
    '''
    def __init__(self, assets, mil_strength, bloc, *args, **kwargs):
        '''
        New CrisisAgent.
        '''
        self.assets = assets
        self.mil_strength = mil_strength
        self.bloc = bloc
        super().__init__(*args, **kwargs)

class RationalAgent(SPEAgent, CrisisAgent):
    def add_utility(self, utility):
        pass


class CrisisWorld(MetaModel):
    '''
    Basic MetaModel for running multiple iterations with a fixed population.

    By default, creates agents with random military strength, assets and
    bloc membership. Fixed agent parameters (e.g. learning rates) 
    can be passed as a dictionary. To generate more complex behaviors, override
    the make_agents method.

    By default, the log is a list of run qualities. Override assess_run method
    as needed to change that.
    '''
    model_class = CrisisModel
    agent_class = CrisisAgent
    agent_count = 10
    agent_args = {}

    def __init__(self, agent_class, agent_count, agent_args={}, seed=None):
        '''
        Instantiate a new CrisisWorld model.

        Args:
            agent_class: Class to instantiate the agents 
            agent_count: How many agents to instantiate with.
            agent_args: Dictionary of arguments to pass to all agents.
            seed: Random seed to launch the model with.
        '''
        self.agent_class = agent_class
        self.agent_count = agent_count
        self.agent_args = agent_args
        super().__init__(self.model_class, agents_per_model=2, seed=seed)
        
        # Instantiate data collector
        self.dc = DataCollector(tables={
                    "Interactions": 
                        ["Step", "A", "B", "Outcome", "SPE", "quality"],
                    "Agents": ["Name", "Assets", "Capability", "Bloc"] })

        for agent in self.agents:
            row = {"Name": agent.name, 
                   "Assets": agent.assets,
                   "Capability": agent.mil_strength,
                   "Bloc": agent.bloc}
            self.dc.add_table_row("Agents", row)

    def make_agents(self):
        '''
        Create self.agent_count agents.
        '''
        self.agents = []
        for i in range(self.agent_count):
            m = random.randrange(1, 100) # Mil strength
            a = random.randrange(1,100) # Assets
            b = random.randrange(2) # Bloc
            agent_args = dict(learning_rate=0.1,
                              discount_factor=0.9,
                              assets=a,
                              mil_strength=m,
                              bloc=b,
                              name=i)
            for arg, val in self.agent_args.items():
                agent_args[arg] = val
            a = self.agent_class(**agent_args)
            self.agents.append(a)

    def step(self):
        '''
        Pair up all agents at random and have them interact.
        '''
        random.shuffle(self.agents)
        for agent in self.agents:
            alters = [a for a in self.agents if a is not agent]
            random.shuffle(alters)
            for alter in alters:
                model = self.model_class([agent, alter])
                model.run()
                self.assess_run(model)
        self.steps += 1

    def assess_run(self, model):
        '''
        Log the model outcome and equilibrium outcome, and compute similarity.
        '''
        spe_model = model.find_equilibrium()
        a, b = model.agents
        q = model.log.tversky_index(spe_model.log)
        row = {"Step": self.steps, "A": a.name, "B": b.name, 
               "Outcome": model.current_node, "SPE": spe_model.current_node,
               "quality": q}
        self.dc.add_table_row("Interactions", row)

