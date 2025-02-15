module.exports = {
  development: {
    client: "pg",
    connection: {
      host: "127.0.0.1",
      port: "5433",
      user: "leo",
      password: "leo",
      database: "org_dev",
    },
    migrations: {
      directory: "./migrations",
    },
    seeds: {
      directory: "./seeds",
    },
  },
  production: {
    client: "pg",
    connection: {
      host: "127.0.0.1",
      port: "5433",
      user: "leo",
      password: "leo",
      database: "org_prod",
    },
    migrations: {
      directory: "./migrations",
    },
    seeds: {
      directory: "./seeds",
    },
  },
  test: {
    client: "pg",
    connection: {
      host: "127.0.0.1",
      port: "5433",
      user: "leo",
      password: "leo",
      database: "org_test",
    },
    migrations: {
      directory: "./migrations",
    },
    seeds: {
      directory: "./seeds",
    },
  },
};
