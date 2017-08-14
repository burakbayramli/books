import tensorflow as tf
import numpy as np
from sklearn.utils import shuffle

# Here we define our cluster setup via the command line
tf.app.flags.DEFINE_string("ps_hosts", "",
                           "Comma-separated list of hostname:port pairs")
tf.app.flags.DEFINE_string("worker_hosts", "",
                           "Comma-separated list of hostname:port pairs")

# Define the characteristics of the cluster node, and its task index
tf.app.flags.DEFINE_string("job_name", "", "One of 'ps', 'worker'")
tf.app.flags.DEFINE_integer("task_index", 0, "Index of task within the job")

FLAGS = tf.app.flags.FLAGS


def main(_):
  ps_hosts = FLAGS.ps_hosts.split(",")
  worker_hosts = FLAGS.worker_hosts.split(",")

  # Create a cluster following the command line paramaters.
  cluster = tf.train.ClusterSpec({"ps": ps_hosts, "worker": worker_hosts})
  
  # Create the local task.
  server = tf.train.Server(cluster,
                           job_name=FLAGS.job_name,
                           task_index=FLAGS.task_index)

  if FLAGS.job_name == "ps":
    server.join()
  elif FLAGS.job_name == "worker":

    # Assigns ops to the local worker by default.
    with tf.device(tf.train.replica_device_setter(
        worker_device="/job:worker/task:%d" % FLAGS.task_index,
        cluster=cluster)):

      #Define the training set, and the model parameters, loss function and training operation
      trX = np.linspace(-1, 1, 101)
      trY = 2 * trX + np.random.randn(*trX.shape) * 0.4 + 0.2 # create a y value
      X = tf.placeholder("float", name="X") # create symbolic variables
      Y = tf.placeholder("float", name = "Y")

      def model(X, w, b):
        return tf.mul(X, w) + b # We just define the line as X*w + b0  

      w = tf.Variable(-1.0, name="b0") # create a shared variable
      b = tf.Variable(-2.0, name="b1") # create a shared variable
      y_model = model(X, w, b)

      loss = (tf.pow(Y-y_model, 2)) # use sqr error for cost function
      global_step = tf.Variable(0)

      train_op = tf.train.AdagradOptimizer(0.8).minimize(
          loss, global_step=global_step)

    #Create a saver, and a summary and init operation
      saver = tf.train.Saver()
      summary_op = tf.merge_all_summaries()
      init_op = tf.initialize_all_variables()

    # Create a "supervisor", which oversees the training process.
    sv = tf.train.Supervisor(is_chief=(FLAGS.task_index == 0),
                             logdir="/tmp/train_logs",
                             init_op=init_op,
                             summary_op=summary_op,
                             saver=saver,
                             global_step=global_step,
                             save_model_secs=600)

    # The supervisor takes care of session initialization, restoring from
    # a checkpoint, and closing when done or an error occurs.
    with sv.managed_session(server.target) as sess:
      # Loop until the supervisor shuts down
      step = 0
      while not sv.should_stop() :
        # Run a training step asynchronously.
        # See `tf.train.SyncReplicasOptimizer` for additional details on how to
        # perform *synchronous* training.
        for i in range(100):
          trX, trY = shuffle (trX, trY, random_state=0)
          for (x, y) in zip(trX, trY): 
              _, step = sess.run([train_op, global_step],feed_dict={X: x, Y: y})
          #Print the partial results, and the current node doing the calculation
          print ("Partial result from node: " + str(FLAGS.task_index) + ", w: " + str(w.eval(session=sess))+ ", b0: " + str(b.eval(session=sess)))
    # Ask for all the services to stop.
    sv.stop()

    
    
if __name__ == "__main__":
  tf.app.run()
