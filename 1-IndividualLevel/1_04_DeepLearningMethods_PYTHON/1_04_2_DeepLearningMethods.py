# from data_generators import *
import numpy as np
import tensorflow as tf
from tensorflow.contrib import layers as L
# from plotting_code import main as plt
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot


def euclidean_loss(a, b):
    return tf.reduce_mean(tf.reduce_sum(tf.pow(a - b, 2))) / tf.cast(tf.shape(a)[0], 'float')

def test_function():
    print(4)

def MSE(a, b):
    return np.mean((a - b) ** 2)

def scaled_sigmoid(x):
            return 2*tf.sigmoid(x) - 1


class CATE_estimator:
    def evaluate(self, X, TAU):
        t_hat = self.predict(X)
        t_dist = MSE(t_hat, TAU)
        return t_dist


class LinearTlearner(CATE_estimator):
    tf_trainable = True
    def __init__(self, XS, YS, WS, catagorial=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        self.nn_input_X1 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_X0 = tf.placeholder("float", [None, XS.shape[1]])
        if catagorial:
            self.nn_input_Y1 = tf.placeholder("int64", [None, ])
            self.nn_input_Y0 = tf.placeholder("int64", [None, ])
        else:
            self.nn_input_Y1 = tf.placeholder("float", [None, YS.shape[1]])
            self.nn_input_Y0 = tf.placeholder("float", [None, YS.shape[1]])

        Y_hat_0 = L.fully_connected(self.nn_input_X0, YS.shape[1],
                                    activation_fn=tf.identity,
                                    weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))
        Y_hat_1 = L.fully_connected(self.nn_input_X1, YS.shape[1],
                                    activation_fn=tf.identity,
                                    weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))

        self.loss0 = euclidean_loss(Y_hat_0, self.nn_input_Y0)
        self.loss1 = euclidean_loss(Y_hat_1, self.nn_input_Y1)
        self.tau_estimate = Y_hat_1 - Y_hat_0

        self.trainer_0 = tf.train.AdamOptimizer().minimize(self.loss0)
        self.trainer_1 = tf.train.AdamOptimizer().minimize(self.loss1)

        self.name = "t_lm"
        self.catagorial = catagorial

    def train(self, X0, Y0, X1, Y1):
        if (len(Y1.shape) == 1):
            Y1 = np.expand_dims(Y1, axis=1)
        if (len(Y0.shape) == 1):
            Y0 = np.expand_dims(Y0, axis=1)
        fd = {self.nn_input_X0: X0, self.nn_input_X1: X1,
              self.nn_input_Y0: Y0, self.nn_input_Y1: Y1}
        sess = tf.get_default_session()
        _, _, l0, l1 = sess.run([self.trainer_0, self.trainer_1, self.loss0, self.loss1],
                                feed_dict=fd)
        return l0 + l1

    def predict(self, X):
        fd = {self.nn_input_X0: X,
              self.nn_input_X1: X,
              self.nn_input_Y0: np.expand_dims(np.zeros(len(X)), axis=1),
              self.nn_input_Y1: np.expand_dims(np.zeros(len(X)), axis=1)}
        sess = tf.get_default_session()
        t_hat = sess.run([self.tau_estimate], feed_dict=fd)[0]
        return np.squeeze(t_hat.flatten())



class Tlearner(CATE_estimator):
    tf_trainable = True

    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        self.nn_input_X1 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_X0 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_Y1 = tf.placeholder("float", [None, YS.shape[1]])
        self.nn_input_Y0 = tf.placeholder("float", [None, YS.shape[1]])

        n_hid = 64

        h0_0 = L.fully_connected(self.nn_input_X0, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))
        h1_0 = L.fully_connected(h0_0, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))
        Y_hat_0 = L.fully_connected(h1_0, YS.shape[1],
                                    activation_fn=activation,
                                    weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))

        h0_1 = L.fully_connected(self.nn_input_X1, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))
        h1_1 = L.fully_connected(h0_1, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))

        Y_hat_1 = L.fully_connected(h1_1, YS.shape[1],
                                    activation_fn=tf.identity,
                                    weights_initializer=tf.contrib.layers.xavier_initializer(uniform=False))
        if link_function:
            Y_hat_1 = tf.sigmoid(Y_hat_1)
            Y_hat_0 = tf.sigmoid(Y_hat_0)

        self.loss0 = euclidean_loss(Y_hat_0, self.nn_input_Y0)
        self.loss1 = euclidean_loss(Y_hat_1, self.nn_input_Y1)
        self.tau_estimate = Y_hat_1 - Y_hat_0

        # self.tau_loss = tf.reduce_mean(tf.reduce_sum(tf.pow(self.tau_estimate - self.tau_ground_truth, 2)))

        self.trainer_0 = tf.train.AdamOptimizer().minimize(self.loss0)
        self.trainer_1 = tf.train.AdamOptimizer().minimize(self.loss1)

        if activation == tf.nn.relu:
            self.name = 't_relu'
        elif activation == tf.nn.tanh:
            self.name = 't_tanh'
        elif activation == tf.nn.sigmoid:
            self.name = 't_sigmoid'

    def train(self, X0, Y0, X1, Y1):
        if (len(Y1.shape) == 1):
            Y1 = np.expand_dims(Y1, axis=1)
        if (len(Y0.shape) == 1):
            Y0 = np.expand_dims(Y0, axis=1)
        fd = {self.nn_input_X0: X0, self.nn_input_X1: X1,
              self.nn_input_Y0: Y0, self.nn_input_Y1: Y1}
        sess = tf.get_default_session()
        _, _, l0, l1 = sess.run([self.trainer_0, self.trainer_1, self.loss0, self.loss1],
                                feed_dict=fd)
        return (l0 + l1) / 2

    def predict(self, X, clip_range=(-1, 1)):
        fd = {self.nn_input_X0: X,
              self.nn_input_X1: X,
              self.nn_input_Y0: np.expand_dims(np.zeros(len(X)), axis=1),
              self.nn_input_Y1: np.expand_dims(np.zeros(len(X)), axis=1)}
        sess = tf.get_default_session()
        t_hat = sess.run([self.tau_estimate], feed_dict=fd)[0]
        #if clip_range is not None:
        #    return np.clip(t_hat.flatten(), clip_range[0], clip_range[1])
        return t_hat.flatten()


class Slearner(CATE_estimator):
    tf_trainable = True

    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        self.nn_input_X = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_W = tf.placeholder("float", [None])
        self.nn_input_XW = tf.concat([self.nn_input_X, tf.expand_dims(self.nn_input_W, axis=-1)], axis=1)

        self.nn_input_Y = tf.placeholder("float", [None, YS.shape[1]])

        n_hid = 64

        h0_0 = L.fully_connected(self.nn_input_XW, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=True))
        h1_0 = L.fully_connected(h0_0, n_hid,
                                 activation_fn=activation,
                                 weights_initializer=tf.contrib.layers.xavier_initializer(uniform=True))

        self.Y_hat = L.fully_connected(h1_0, YS.shape[1],
                                       activation_fn=tf.identity)

        if link_function:
            self.Y_hat = tf.sigmoid(self.Y_hat)

        self.loss = euclidean_loss(self.Y_hat,
                                   self.nn_input_Y)  # tf.reduce_mean(tf.reduce_sum(tf.pow(self.Y_hat - self.nn_input_Y, 2)))

        self.trainer = tf.train.AdamOptimizer().minimize(self.loss)

        if activation == tf.nn.relu:
            self.name = 's_relu'
        elif activation == tf.nn.tanh:
            self.name = 's_tanh'

    def train(self, X_mb, Y_mb, W_mb):
        if (len(Y_mb.shape) == 1):
            Y_mb = np.expand_dims(Y_mb, axis=1)
        fd = {self.nn_input_X: X_mb,
              self.nn_input_Y: Y_mb,
              self.nn_input_W: W_mb}
        sess = tf.get_default_session()
        _, l = sess.run([self.trainer, self.loss],
                        feed_dict=fd)
        return l

    def predict(self, X, clip_range=(-1, 1)):
        sess = tf.get_default_session()
        fd = {self.nn_input_X: X,
              self.nn_input_Y: np.expand_dims(np.zeros(len(X)), axis=1),
              self.nn_input_W: np.expand_dims(np.zeros(len(X)), axis=1)}
        W0 = np.zeros(X.shape[0])
        W1 = np.ones(X.shape[0])
        fd[self.nn_input_W] = W0

        mu_hat_x_0 = sess.run([self.Y_hat], feed_dict=fd)[0]
        fd[self.nn_input_W] = W1
        mu_hat_x_1 = sess.run([self.Y_hat], feed_dict=fd)[0]
        tau_hat = mu_hat_x_1 - mu_hat_x_0
        #if clip_range is not None:
        #    return np.clip(tau_hat.flatten(), clip_range[0], clip_range[1])
        return tau_hat.flatten()


class YLearner(CATE_estimator):
    tf_trainable = True

    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        self.nn_input_X = tf.placeholder("float", [None, XS.shape[1]])
        # self.nn_input_Y = tf.placeholder("float", [None, YS.shape[1]])
        # self.nn_input_X0 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_sigma_0_ground_truth = tf.placeholder("float", [None, YS.shape[1]])
        self.nn_input_sigma_1_grond_truth = tf.placeholder("float", [None, YS.shape[1]])
        self.nn_input_sigma_tau_ground_truth = tf.placeholder("float", [None, YS.shape[1]])

        n_hid = 64
        if activation == tf.nn.relu:
            self.name = 'y_relu'
        elif activation == tf.nn.tanh:
            self.name = 'y_tanh'

        h0_sigma_0 = L.fully_connected(self.nn_input_X, n_hid,
                                       activation_fn=activation)
        h1_sigma_0 = L.fully_connected(h0_sigma_0, n_hid,
                                       activation_fn=activation)

        h0_sigma_1 = L.fully_connected(self.nn_input_X, n_hid,
                                       activation_fn=activation)
        h1_sigma_1 = L.fully_connected(h0_sigma_1, n_hid,
                                       activation_fn=activation)

        h0_sigma_tau = L.fully_connected(self.nn_input_X, n_hid,
                                         activation_fn=activation)
        h1_sigma_tau = L.fully_connected(h0_sigma_tau, n_hid,
                                         activation_fn=activation)

        self.sigma_0_hat = L.fully_connected(h1_sigma_0, YS.shape[1],
                                             activation_fn=tf.identity)

        self.sigma_1_hat = L.fully_connected(h1_sigma_1, YS.shape[1],
                                             activation_fn=tf.identity)

        self.sigma_tau_hat = L.fully_connected(h1_sigma_tau, YS.shape[1],
                                               activation_fn=tf.identity)

        if link_function:
            self.sigma_0_hat = tf.sigmoid(self.sigma_0_hat)
            self.sigma_1_hat = tf.sigmoid(self.sigma_1_hat)
            self.sigma_tau_hat = tf.nn.tanh(self.sigma_tau_hat)

        loss_one = euclidean_loss(self.sigma_0_hat, self.nn_input_sigma_0_ground_truth)
        loss_two = euclidean_loss(self.sigma_1_hat, self.nn_input_sigma_1_grond_truth)
        loss_three = euclidean_loss(self.sigma_tau_hat, self.nn_input_sigma_tau_ground_truth)
        self.loss = loss_one + loss_two + loss_three
        self.trainer = tf.train.AdamOptimizer().minimize(self.loss)

    def train(self, X_mb, Y_mb, W_mb):
        if (len(Y_mb.shape) == 1):
            Y_mb = np.expand_dims(Y_mb, axis=1)
        sigma_0, sigma_1, sigma_tau = self.generate_labels(X_mb, Y_mb, W_mb)
        fd = {self.nn_input_X: X_mb,
              self.nn_input_sigma_0_ground_truth: sigma_0,
              self.nn_input_sigma_1_grond_truth: sigma_1,
              self.nn_input_sigma_tau_ground_truth: sigma_tau}
        sess = tf.get_default_session()
        _, l = sess.run([self.trainer, self.loss],
                        feed_dict=fd)
        return l

    def predict(self, X, clip_range=(-1, 1)):
        tau_hat = self.get_sigma(self.sigma_tau_hat, X)
        #if clip_range is not None:
        #    return np.clip(tau_hat, clip_range[0], clip_range[1])
        return tau_hat

    def get_sigma(self, sigma, X):
        sess = tf.get_default_session()
        fd = {self.nn_input_X: X}
        out = sess.run([sigma], feed_dict=fd)[0]
        out = out.flatten()  # Maybe??
        # print(out.shape)
        return out

    def generate_labels(self, X, Y, W):
        # print(X.shape)
        # X = X.flatten()
        # print(X.shape)
        Y = Y.flatten()
        W = W.flatten()
        sigma_0 = np.zeros_like(Y)
        sigma_1 = np.zeros_like(Y)
        sigma_tau = np.zeros_like(Y)
        W_0 = np.where(W == 0)
        W_1 = np.where(W == 1)
        sigma_0[W_0] = Y[W_0]
        # print(sigma_0[W_1].flatten().shape)
        # print(Y[W_1].flatten().shape)
        # print(self.get_sigma(self.sigma_tau_hat, X[W_1]).shape)
        sigma_0[W_1] = Y[W_1] - self.get_sigma(self.sigma_tau_hat, X[W_1])
        sigma_1[W_1] = Y[W_1]
        sigma_1[W_0] = Y[W_0] + self.get_sigma(self.sigma_tau_hat, X[W_0])
        sigma_tau[W_1] = Y[W_1] - self.get_sigma(self.sigma_0_hat, X[W_1])
        sigma_tau[W_0] = self.get_sigma(self.sigma_1_hat, X[W_0]) - Y[W_0]
        sigma_0 = np.expand_dims(sigma_0, -1)
        sigma_1 = np.expand_dims(sigma_1, -1)
        sigma_tau = np.expand_dims(sigma_tau, -1)
        return sigma_0, sigma_1, sigma_tau


class Xlearner(CATE_estimator):
    tf_trainable = True

    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        self.nn_input_X1 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_Y1 = tf.placeholder("float", [None, YS.shape[1]])
        self.nn_input_X0 = tf.placeholder("float", [None, XS.shape[1]])
        self.nn_input_Y0 = tf.placeholder("float", [None, YS.shape[1]])

        self.nn_input_D_hat_0 = tf.placeholder("float", [None, YS.shape[1]])
        self.nn_input_D_hat_1 = tf.placeholder("float", [None, YS.shape[1]])

        n_hid = 64

        if activation == tf.nn.relu:
            self.name = 'x_relu'
        elif activation == tf.nn.tanh:
            self.name = 'x_tanh'

        def get_weights_and_biases(n_layers, in_shape, out_shape):
            w_dict = dict()
            b_dict = dict()
            n_units = n_hid
            for i in range(n_layers):
                if i == n_layers - 1:
                    n_units = out_shape
                w_dict['h_' + str(i)] = tf.Variable(tf.random_normal([in_shape, n_units]))
                b_dict['b_' + str(i)] = tf.Variable(tf.random_normal([n_units]))
                in_shape = n_hid
            return w_dict, b_dict

        def MLP(nn_input, weights, biases):
            n_layers = len(weights)
            l = nn_input
            for i in range(n_layers):
                if i == n_layers - 1:
                    Y_hat = tf.matmul(l, weights['h_' + str(i)]) + biases['b_' + str(i)]
                    # print("no_relu")
                else:
                    l = activation(tf.matmul(l, weights['h_' + str(i)]) + biases['b_' + str(i)])

            return Y_hat

        weights_0, biases_0 = get_weights_and_biases(3, XS.shape[1], YS.shape[1])
        weights_1, biases_1 = get_weights_and_biases(3, XS.shape[1], YS.shape[1])

        mu_hat_0 = MLP(self.nn_input_X0, weights_0, biases_0)
        mu_hat_1 = MLP(self.nn_input_X1, weights_1, biases_1)

        if link_function:
            mu_hat_0 = tf.sigmoid(mu_hat_0)
            mu_hat_1 = tf.sigmoid(mu_hat_1)

        self.D_hat_1 = self.nn_input_Y1 - MLP(self.nn_input_X1, weights_0, biases_0)
        self.D_hat_0 = MLP(self.nn_input_X0, weights_1, biases_1) - self.nn_input_Y0

        # D_hat_0_stopped = tf.stop_gradient(D_hat_0)
        # D_hat_1_stopped = tf.stop_gradient(D_hat_1)

        tau_hat_0 = L.fully_connected(self.nn_input_X0, n_hid,
                                      activation_fn=activation)
        # tau_hat_0 = L.dropout(tau_hat_0, 0.3)
        # tau_hat_0 = L.batch_norm(tau_hat_0)
        tau_hat_0 = L.fully_connected(tau_hat_0, n_hid,
                                      activation_fn=activation)
        # tau_hat_0 = L.dropout(tau_hat_0, 0.3)
        # tau_hat_0 = L.batch_norm(tau_hat_0)
        tau_hat_0 = L.fully_connected(tau_hat_0, YS.shape[1], activation_fn=tf.identity)

        tau_hat_1 = L.fully_connected(self.nn_input_X1, n_hid,
                                      activation_fn=activation)
        # tau_hat_1 = L.dropout(tau_hat_1, 0.3)
        # tau_hat_1 = L.batch_norm(tau_hat_1)
        tau_hat_1 = L.fully_connected(tau_hat_1, n_hid,
                                      activation_fn=activation)
        # tau_hat_1 = L.dropout(tau_hat_1, 0.3)
        # tau_hat_1 = L.batch_norm(tau_hat_1)
        tau_hat_1 = L.fully_connected(tau_hat_1, YS.shape[1], activation_fn=tf.identity)

        if link_function:
            tau_hat_0 = tf.nn.tanh(tau_hat_0)
            tau_hat_1 = tf.nn.tanh(tau_hat_1)

        self.tau_hat_0 = tau_hat_0
        self.tau_hat_1 = tau_hat_1

        self.loss_tau_0 = euclidean_loss(tau_hat_0, self.nn_input_D_hat_0)
        self.loss_tau_1 = euclidean_loss(tau_hat_1, self.nn_input_D_hat_1)
        self.loss_mu_0 = euclidean_loss(mu_hat_0, self.nn_input_Y0)
        self.loss_mu_1 = euclidean_loss(mu_hat_1, self.nn_input_Y1)

        self.trainer_0 = tf.train.AdamOptimizer().minimize(self.loss_mu_0)
        self.trainer_1 = tf.train.AdamOptimizer().minimize(self.loss_mu_1)

        self.trainer_tau_0 = tf.train.AdamOptimizer().minimize(self.loss_tau_0)
        self.trainer_tau_1 = tf.train.AdamOptimizer().minimize(self.loss_tau_1)

    def train(self, X0, Y0, X1, Y1):
        if (len(Y1.shape) == 1):
            Y1 = np.expand_dims(Y1, axis=1)
        if (len(Y0.shape) == 1):
            Y0 = np.expand_dims(Y0, axis=1)
        # To DO: Compare against TAU and return results.
        fd = {self.nn_input_X0: X0, self.nn_input_X1: X1,
              self.nn_input_Y0: Y0, self.nn_input_Y1: Y1}
        sess = tf.get_default_session()
        sess.run([self.trainer_0, self.trainer_1], feed_dict=fd)
        D_0, D_1 = sess.run([self.D_hat_0, self.D_hat_1], feed_dict=fd)
        fd[self.nn_input_D_hat_1] = D_1
        fd[self.nn_input_D_hat_0] = D_0

        _, _, l0t, l1t = sess.run([self.trainer_tau_0, self.trainer_tau_1, self.loss_tau_0, self.loss_tau_1],
                                  feed_dict=fd)
        return (l0t + l1t) / 2

    def predict(self, X, clip_range=(-1, 1)):
        fd = {self.nn_input_X0: X, self.nn_input_X1: X,
              self.nn_input_Y0: np.expand_dims(np.zeros(len(X)), axis=1),
              self.nn_input_Y1: np.expand_dims(np.zeros(len(X)), axis=1)}
        sess = tf.get_default_session()
        D_0, D_1 = sess.run([self.D_hat_0, self.D_hat_1], feed_dict=fd)
        fd[self.nn_input_D_hat_1] = D_1
        fd[self.nn_input_D_hat_0] = D_0
        t_0, t_1 = sess.run([self.tau_hat_0, self.tau_hat_1],
                            feed_dict=fd)
        t_hat = 0.5 * t_0 + 0.5 * t_1

        #if clip_range is not None:
        #    return np.clip(t_hat.flatten(), clip_range[0], clip_range[1])

        return t_hat.flatten()


class Ulearner(CATE_estimator):
    tf_trainable = True
    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        WS = WS.reshape([-1, 1])
        self.nn_input_X = tf.placeholder("float64", [None, XS.shape[1]])
        self.nn_input_W = tf.placeholder("float64", [None, 1])
        self.nn_input_Y = tf.placeholder("float64", [None, YS.shape[1]])

        self.slice = tf.placeholder("int32")
        self.batch_size = tf.placeholder("int32")
        # self.e_hat_x = tf.placeholder("float", [None, 1])
        # self.mu_obs = tf.placeholder("float", [None,1])
        n_hid = 64
        if activation == tf.nn.relu:
            self.name = 'u_relu'
        elif activation == tf.nn.tanh:
            self.name = 'u_tanh'

        def get_weights_and_biases(n_layers, in_shape, out_shape):
            w_dict = dict()
            b_dict = dict()
            n_units = n_hid
            for i in range(n_layers):
                if i == n_layers - 1:
                    n_units = out_shape
                w_dict['h_' + str(i)] = tf.Variable(tf.random_normal([in_shape, n_units], dtype=tf.float64))
                b_dict['b_' + str(i)] = tf.Variable(tf.random_normal([n_units], dtype=tf.float64))
                in_shape = n_hid
            return w_dict, b_dict

        def MLP(nn_input, weights, biases):
            n_layers = len(weights)
            l = nn_input
            for i in range(n_layers):
                if i == n_layers - 1:
                    Y_hat = tf.matmul(l, weights['h_' + str(i)]) + biases['b_' + str(i)]
                else:
                    l = activation(tf.matmul(l, weights['h_' + str(i)]) + biases['b_' + str(i)])

            return Y_hat

        ## Estimate mu obs (expected value of Y_obs given X)(Predict Y) with M_obs: multilayer perceptron
        M_obs_weights, M_obs_biases = get_weights_and_biases(3, XS.shape[1], YS.shape[1])
        self.mu_hat_obs = MLP(self.nn_input_X, M_obs_weights, M_obs_biases)
        ## ~
        mu_hat_obs_stopped = tf.stop_gradient(self.mu_hat_obs)

        ## Find e_hat using M_e: Random Forest Regressor
        self.rf = RandomForestRegressor(n_estimators=200, oob_score=True, verbose=1)
        self.rf.fit(XS, np.ravel(WS))
        self.ehat_oob = self.rf.oob_prediction_

        self.e_hat_x = tf.reshape(tf.gather(self.ehat_oob, tf.range(self.slice, self.slice + self.batch_size)), [-1, 1])

        ## Calculate R
        self.R = (self.nn_input_Y - self.mu_hat_obs) / tf.clip_by_value((self.nn_input_W - self.e_hat_x), 1e-6, 1-1e-6)

        ## Compute M_tau on X to estimate R
        h0_1 = L.fully_connected(self.nn_input_X, n_hid, activation_fn=activation)
        h1_1 = L.fully_connected(h0_1, n_hid, activation_fn=activation)
        self.tau_hat = L.fully_connected(h1_1, int(self.R.shape[1]), activation_fn=tf.identity)

        if link_function:
            self.tau_hat = tf.nn.tanh(self.tau_hat)

        ## Train Mu Loss
        self.loss_mu_obs = tf.reduce_mean(tf.reduce_sum(tf.pow(self.mu_hat_obs - self.nn_input_Y, 2)))
        self.mu_obs_trainer = tf.train.AdamOptimizer().minimize(self.loss_mu_obs)

        ## Train Tau Loss on predicting R from X
        self.tau_loss = tf.reduce_mean(tf.reduce_sum(tf.pow(self.tau_hat - self.R, 2)))
        self.tau_trainer = tf.train.AdamOptimizer().minimize(self.tau_loss)

    def train(self, X_mb, Y_mb, W_mb, slice_num, batch_size):
        if (len(Y_mb.shape) == 1):
            Y_mb = np.expand_dims(Y_mb, axis=1)
        if (len(W_mb.shape) == 1):
            W_mb = np.expand_dims(W_mb, axis=1)
        fd = {self.nn_input_X: X_mb,
              self.nn_input_Y: Y_mb,
              self.nn_input_W: W_mb,
              self.slice: slice_num,
              self.batch_size: batch_size}
        sess = tf.get_default_session()
        #print("EHAT", sess.run(self.R, feed_dict = fd))
        _, _, t_l = sess.run([self.mu_obs_trainer, self.tau_trainer, self.tau_loss],
                             feed_dict=fd)
        return t_l

    def predict(self, X):
        fd = {self.nn_input_X: X}
        sess = tf.get_default_session()
        t_hat = sess.run(self.tau_hat,
                         feed_dict=fd)
        #print("T_HAT", t_hat)
        return t_hat.flatten()


class Flearner(CATE_estimator):
    tf_trainable = True
    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function=False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        WS = WS.reshape([-1, 1])
        self.nn_input_X = tf.placeholder("float64", [None, XS.shape[1]], name = "X")
        self.nn_input_W = tf.placeholder("float64", [None, 1], name = "W")
        self.nn_input_Y = tf.placeholder("float64", [None, YS.shape[1]], name = "Y")
        self.slice = tf.placeholder("int32", name = "slice")
        self.batch_size = tf.placeholder("int32", name = "batch")
        #self.e_hat_x = tf.placeholder("float", [None, 1])
        self.rf = RandomForestRegressor(n_estimators=200, oob_score=True, verbose=1)
        self.rf.fit(XS, np.ravel(WS))
        self.ehat_oob = self.rf.oob_prediction_
        self.e_hat_x = tf.reshape(tf.gather(self.ehat_oob, tf.range(self.slice, self.slice + self.batch_size)), [-1, 1])
        self.e_hat_x = tf.clip_by_value(self.e_hat_x, 1e-6, 1-1e-6)

        Y_star = self.nn_input_Y * ((self.nn_input_W - self.e_hat_x) / ((self.e_hat_x) * (1 - self.e_hat_x)))

        n_hid = 64

        h0_1 = L.fully_connected(self.nn_input_X, n_hid, activation_fn=activation)
        h1_1 = L.fully_connected(h0_1, n_hid, activation_fn=activation)
        self.tau_hat = L.fully_connected(h1_1, int(Y_star.shape[1]), activation_fn=tf.identity)
        if link_function:
            self.tau_hat = tf.nn.tanh(self.tau_hat)

        self.tau_loss = tf.reduce_mean(tf.reduce_sum(tf.pow(self.tau_hat - Y_star, 2)))
        self.tau_trainer = tf.train.AdamOptimizer().minimize(self.tau_loss)

        if activation == tf.nn.relu:
            self.name = 'f_relu'
        elif activation == tf.nn.tanh:
            self.name = 'f_tanh'

    def train(self, X_mb, Y_mb, W_mb, slice_num, batch_size):
        if (len(Y_mb.shape) == 1):
            Y_mb = np.expand_dims(Y_mb, axis=1)
        if (len(W_mb.shape) == 1):
            W_mb = np.expand_dims(W_mb, axis=1)
        fd = {self.nn_input_X: X_mb,
              self.nn_input_Y: Y_mb,
              self.nn_input_W: W_mb,
              self.slice: slice_num,
              self.batch_size: batch_size}
        sess = tf.get_default_session()
        _, t_l = sess.run([self.tau_trainer, self.tau_loss],
                          feed_dict=fd)
        return t_l

    def predict(self, X):
        fd = {self.nn_input_X: X}
        sess = tf.get_default_session()
        t_hat = sess.run(self.tau_hat, feed_dict=fd)
        return t_hat.flatten()


class RLearner(CATE_estimator):
    tf_trainable = True
    def __init__(self, XS, YS, WS, activation=tf.nn.tanh, link_function = False):
        if (len(YS.shape) == 1):
            YS = np.expand_dims(YS, axis=1)
        WS = WS.reshape([-1, 1])
        self.nn_input_X = tf.placeholder("float64", [None, XS.shape[1]])
        self.nn_input_W = tf.placeholder("float64", [None, 1])
        self.nn_input_Y = tf.placeholder("float64", [None, YS.shape[1]])
        self.slice = tf.placeholder(tf.int32)
        self.batch_size = tf.placeholder(tf.int32)
        #self.e_hat_x = tf.placeholder("float", [None, 1])
        self.rf = RandomForestRegressor(n_estimators=500, oob_score=True, verbose=1)
        self.rf.fit(XS, np.ravel(WS))
        self.ehat_oob = self.rf.oob_prediction_
        #self.e_hat_x = self.ehat_oob[self.slice, self.slice + self.batch_size]
        self.e_hat_x = tf.reshape(tf.gather(self.ehat_oob, tf.range(self.slice, self.slice + self.batch_size)), [-1, 1])
        #print("EHAT", self.e_hat_x, self.nn_input_W, self.e_hat_x.get_shape())
        #print("TEST", self.nn_input_W - self.e_hat_x)

        self.rf_m = RandomForestRegressor(n_estimators=500, oob_score=True, verbose=1)
        self.rf_m.fit(XS, np.ravel(YS))
        self.m_hat_oob = self.rf_m.oob_prediction_

        #self.m_hat_x = self.m_hat_oob[self.slice, self.slice + self.batch_size]
        self.m_hat_x = tf.reshape(tf.gather(self.m_hat_oob, tf.range(self.slice, self.slice + self.batch_size)), [-1, 1])

        

        #self.m_hat_x = tf.placeholder("float", [None, YS.shape[1]])

        n_hid = 64

        h0_1 = L.fully_connected(self.nn_input_X, n_hid, activation_fn=activation)
        h1_1 = L.fully_connected(h0_1, n_hid, activation_fn=activation)
        self.tau_hat = L.fully_connected(h1_1, int(self.nn_input_Y.shape[1]), activation_fn=tf.identity)
        if link_function:
            self.tau_hat = tf.nn.tanh(self.tau_hat)

        self.tau_loss = tf.reduce_mean(
            ((self.nn_input_Y - self.m_hat_x) - (self.nn_input_W - self.e_hat_x) * self.tau_hat) ** 2)
        self.tau_trainer = tf.train.AdamOptimizer().minimize(self.tau_loss)

        if activation == tf.nn.relu:
            self.name = 'r_relu'
        elif activation == tf.nn.tanh:
            self.name = 'r_tanh'

    def train(self, X_mb, Y_mb, W_mb, slice_num, batch_size): #E_hat_mb, m_hat_mb):
        if (len(Y_mb.shape) == 1):
            Y_mb = np.expand_dims(Y_mb, axis=1)
        if (len(W_mb.shape) == 1):
            W_mb = np.expand_dims(W_mb, axis=1)
        fd = {self.nn_input_X: X_mb,
              self.nn_input_Y: Y_mb,
              self.nn_input_W: W_mb,
              self.slice: slice_num,
              self.batch_size: batch_size}
        sess = tf.get_default_session()
        _, t_l = sess.run([self.tau_trainer, self.tau_loss],
                          feed_dict=fd)
        return t_l

    def predict(self, X):
        fd = {self.nn_input_X: X}
        sess = tf.get_default_session()
        t_hat = sess.run(self.tau_hat,
                         feed_dict=fd)
        return t_hat.flatten()
if __name__ == '__main__':
    print(3 + 3)
    test_function()