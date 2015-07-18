#!/bin/sh


wget http://spamassassin.apache.org/publiccorpus/20021010_easy_ham.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20021010_hard_ham.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20021010_spam.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20030228_easy_ham.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20030228_easy_ham_2.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20030228_hard_ham.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20030228_spam.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20030228_spam_2.tar.bz2
wget http://spamassassin.apache.org/publiccorpus/20050311_spam_2.tar.bz2

for arch in *bz2
    do tar xf $arch
    done
