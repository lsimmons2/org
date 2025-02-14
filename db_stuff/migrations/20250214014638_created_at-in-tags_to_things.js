exports.up = async function (knex) {
  await knex.schema.alterTable("tags_to_things", (table) => {
    table.timestamp("created_at").defaultTo(knex.fn.now()).notNullable();
  });
};

exports.down = function (knex) {
  return knex.schema.alterTable("tags_to_things", (table) => {
    table.dropColumn("created_at");
  });
};
