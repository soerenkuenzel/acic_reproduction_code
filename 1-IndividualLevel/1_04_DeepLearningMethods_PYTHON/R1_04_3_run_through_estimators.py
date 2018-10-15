from R1_04_1_dataset_containers import full_data
import numpy as np
import tensorflow as tf
import matplotlib.pyplot

from R1_04_2_DeepLearningMethods import *


def train(XS_train, YS_train, WS_train, TAUS_train, Learner, XS_test, WS_test, YS_test, TAUS_test, train_iters,
          E_HAT_train=-1, m_hat_train=-1):
    zero_idxs = np.where(WS_train == 0)
    one_idxs = np.where(WS_train == 1)
    X0 = XS_train[zero_idxs]
    Y0 = YS_train[zero_idxs]
    X1 = XS_train[one_idxs]
    Y1 = YS_train[one_idxs]

    batch_size = 256
    training_losses = []
    tau_evaluated_losses = []
    training_loss = 0.0
    tau_evaluated_loss = 0.0

    for i in range(train_iters):
        if Learner.name[0] == "s" or Learner.name[0] == "y":
            X_slice = i * batch_size % (XS_train.shape[0] - batch_size)
            X_mb = XS_train[X_slice:X_slice + batch_size]
            Y_mb = YS_train[X_slice:X_slice + batch_size]
            W_mb = WS_train[X_slice:X_slice + batch_size]
            training_loss = Learner.train(X_mb, Y_mb, W_mb)
            # TAU_mb = TAUS[X_slice:X_slice + batch_size]
        elif Learner.name[0] == "f" or Learner.name[0] == 'u':
            X_slice = i * batch_size % (XS_train.shape[0] - batch_size)
            X_mb = XS_train[X_slice:X_slice + batch_size]
            Y_mb = YS_train[X_slice:X_slice + batch_size]
            W_mb = WS_train[X_slice:X_slice + batch_size].reshape([-1, 1])
            print(E_HAT_train.shape)
            E_hat_mb = E_HAT_train[X_slice:X_slice + batch_size].reshape([-1, 1])
            training_loss = Learner.train(X_mb, Y_mb, W_mb, E_hat_mb)
        elif Learner.name[0] == "r":
            #print(E_HAT_train)
            X_slice = i * batch_size % (XS_train.shape[0] - batch_size)
            X_mb = XS_train[X_slice:X_slice + batch_size]
            Y_mb = YS_train[X_slice:X_slice + batch_size]
            W_mb = WS_train[X_slice:X_slice + batch_size].reshape([-1, 1])
            E_hat_mb = E_HAT_train[X_slice:X_slice + batch_size].reshape([-1, 1])
            m_hat_mb = m_hat_train[X_slice:X_slice + batch_size].reshape([-1, 1])
            training_loss = Learner.train(X_mb, Y_mb, W_mb, E_hat_mb, m_hat_mb)
        else:
            X0_slice = i * batch_size % (X0.shape[0] - batch_size)
            X1_slice = i * batch_size % (X1.shape[0] - batch_size)
            X0_mb = X0[X0_slice:X0_slice + batch_size]
            X1_mb = X1[X1_slice:X1_slice + batch_size]
            Y0_mb = Y0[X0_slice:X0_slice + batch_size]
            Y1_mb = Y1[X1_slice:X1_slice + batch_size]
            training_loss = Learner.train(X0_mb, Y0_mb, X1_mb, Y1_mb)

        if i % 100 == 0:
            tau_evaluated_loss = Learner.evaluate(XS_test, YS_test,
                                                  WS_test, TAUS_test)
            training_losses.append(training_loss)
            tau_evaluated_losses.append(tau_evaluated_loss)
            print(training_loss, tau_evaluated_loss)

    np.save(Learner.name + "_learn_train", np.array(tau_evaluated_losses))
    plt([np.array(tau_evaluated_losses)], ['Tau losses'])
    plt([np.array(training_losses)], ['Training losses'])

    return Learner.evaluate(XS_train, YS_train, WS_train, TAUS_train), Learner.evaluate(XS_test, YS_test, WS_test,
                                                                                        eTAUS_test)


def run_exp(Learner, num_points, simulation, seed, dim, activation, n_test=10000):
    simulator = simulation(d=dim, seed=seed)
    XS, WS, YS, TAUS = data_generator(n_points=num_points, dim=dim, simulator=simulator)
    X_test, W_test, Y_test, TAU_test = data_generator(n_points=n_test, dim=dim, simulator=simulator)
    learner = Learner(XS, YS, WS, activation)
    if learner.name[0] == 'f' or learner.name[0] == 'u' or learner.name[0] == 'r':
        E_HAT = learner.ehat_oob
    if learner.name[0] == 'r':
        m_hat = learner.m_hat_oob
    init = tf.global_variables_initializer()
    sess = tf.Session()
    with sess.as_default():
        # Run the initializer
        sess.run(init)
        if learner.name[0] == 'f' or learner.name[0] == 'u':
            return train(XS, YS, WS, TAUS, learner, X_test, W_test, Y_test, TAU_test, train_iters=5000,
                         E_HAT_train=E_HAT), learner.name
        elif learner.name[0] == 'r':
            return train(XS, YS, WS, TAUS, learner, X_test, W_test, Y_test, TAU_test, train_iters=5000,
                         E_HAT_train=E_HAT, m_hat_train = m_hat), learner.name
        else:
            return train(XS, YS, WS, TAUS, learner, X_test, W_test, Y_test, TAU_test, train_iters=5000), learner.name



if __name__ == '__main__':
    print(3)
    m = full_data()
    y, w, feat = m.get_train_data()
    print(y)

    # Train:
    Y_train, W_train, X_train = m.get_train_data()
    Y_test, W_test, X_test = m.get_test_data()

    for Learner in [LinearTlearner, Tlearner, Slearner, YLearner, Xlearner, RLearner]:
        learner = Learner(XS = X_train, YS = Y_train, WS = W_train)
        print("Running" + learner.name)

        init = tf.global_variables_initializer()
        sess = tf.Session()
        with sess.as_default():
            sess.run(init)

            zero_idxs = np.where(W_train == 0)
            one_idxs = np.where(W_train == 1)
            X0 = X_train[zero_idxs]
            Y0 = Y_train[zero_idxs]
            X1 = X_train[one_idxs]
            Y1 = Y_train[one_idxs]

            batch_size = 256
            training_losses = []
            tau_evaluated_losses = []
            training_loss = 0.0
            tau_evaluated_loss = 0.0

            print(99)
            train_iters = 1000
            for i in range(train_iters):
                if learner.name[0] == "s" or learner.name[0] == "y":
                    X_slice = i * batch_size % (X_train.shape[0] - batch_size)
                    X_mb = X_train[X_slice:X_slice + batch_size]
                    Y_mb = Y_train[X_slice:X_slice + batch_size]
                    W_mb = W_train[X_slice:X_slice + batch_size]
                    training_loss = learner.train(X_mb, Y_mb, W_mb)
                    # TAU_mb = TAUS[X_slice:X_slice + batch_size]
                elif learner.name[0] == "f" or learner.name[0] == 'u':
                    X_slice = i * batch_size % (X_train.shape[0] - batch_size)
                    X_mb = X_train[X_slice:X_slice + batch_size]
                    Y_mb = Y_train[X_slice:X_slice + batch_size]
                    W_mb = W_train[X_slice:X_slice + batch_size].reshape([-1, 1])
                    print(E_HAT_train.shape)
                    E_hat_mb = E_HAT_train[X_slice:X_slice + batch_size].reshape([-1, 1])
                    training_loss = learner.train(X_mb, Y_mb, W_mb, E_hat_mb)
                elif learner.name[0] == "r":
                    # print(E_HAT_train)
                    X_slice = i * batch_size % (X_train.shape[0] - batch_size)
                    X_mb = X_train[X_slice:X_slice + batch_size]
                    Y_mb = Y_train[X_slice:X_slice + batch_size]
                    W_mb = W_train[X_slice:X_slice + batch_size].reshape([-1, 1])
                    E_HAT_train = learner.ehat_oob
                    m_hat_train = learner.m_hat_oob
                    E_hat_mb = E_HAT_train[X_slice:X_slice + batch_size].reshape([-1, 1])
                    m_hat_mb = m_hat_train[X_slice:X_slice + batch_size].reshape([-1, 1])
                    training_loss = learner.train(X_mb, Y_mb, W_mb, E_hat_mb, m_hat_mb)
                else:
                    X0_slice = i * batch_size % (X0.shape[0] - batch_size)
                    X1_slice = i * batch_size % (X1.shape[0] - batch_size)
                    X0_mb = X0[X0_slice:X0_slice + batch_size]
                    X1_mb = X1[X1_slice:X1_slice + batch_size]
                    Y0_mb = Y0[X0_slice:X0_slice + batch_size]
                    Y1_mb = Y1[X1_slice:X1_slice + batch_size]
                    training_loss = learner.train(X0_mb, Y0_mb, X1_mb, Y1_mb)

            pdts = np.array(learner.predict(X_test))

        tosave = (np.array([np.array(range(1, len(pdts) + 1)), pdts])).transpose()
        np.savetxt('../' + 'estimates/' + learner.name + '.csv', tosave, fmt='%.2f', delimiter=',', header= " ," + learner.name)
