DROP TABLE NEWS_POLITICS;
CREATE TABLE NEWS_POLITICS(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);

DROP TABLE NEWS_ECON;
CREATE TABLE NEWS_ECON(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);

DROP TABLE NEWS_IT;
CREATE TABLE NEWS_IT(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);

DROP TABLE NEWS_LIFE_CULT;
CREATE TABLE NEWS_LIFE_CULT(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);

DROP TABLE NEWS_SOC;
CREATE TABLE NEWS_SOC(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);

DROP TABLE NEWS_WORLD;
CREATE TABLE NEWS_WORLD(
    NEWSID varchar(16) PRIMARY KEY,
    NEWSRANK INT,    
    SRC varchar(32),
    NEWSDATE varchar(16),
    NVIEW INT,
    NCOMMENT INT,
    CURR_CMT INT,
    DELETED INT,
    BROKEN INT,
    MALER INT,
    FEMALER INT,
    X10 INT,
    X20 INT,
    X30 INT,
    X40 INT,
    X50 INT,
    X60 INT
);