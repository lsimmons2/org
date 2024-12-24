module.exports = {
  development: {
    client: "pg",
    connection: {
      host: "127.0.0.1",
      port: "5433",
      user: "leo",
      password: "leo",
      database: "org",
    },
    migrations: {
      directory: "./migrations",
    },
    seeds: {
      directory: "./seeds",
    },
  },
};
