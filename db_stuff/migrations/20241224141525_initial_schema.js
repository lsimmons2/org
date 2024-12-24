exports.up = async function (knex) {
  // Create 'things' table
  await knex.schema.createTable("things", (table) => {
    table.increments("id").primary();
    table.string("name").notNullable();
    table.text("text");
    table.timestamp("created_at").defaultTo(knex.fn.now()).notNullable();
    table.timestamp("updated_at").defaultTo(knex.fn.now());
    table.timestamp("last_viewed_at");
  });

  // Create 'tags' table
  await knex.schema.createTable("tags", (table) => {
    table.increments("id").primary();
    table.string("name").notNullable();
    table.text("text");
    table.timestamp("created_at").defaultTo(knex.fn.now()).notNullable();
    table.timestamp("updated_at").defaultTo(knex.fn.now());
    table
      .integer("parent_tag_id")
      .unsigned()
      .references("id")
      .inTable("tags")
      .onDelete("SET NULL");
  });

  // Create 'tags_to_things' table
  await knex.schema.createTable("tags_to_things", (table) => {
    table.increments("id").primary();
    table
      .integer("thing_id")
      .unsigned()
      .references("id")
      .inTable("things")
      .onDelete("CASCADE");
    table
      .integer("tag_id")
      .unsigned()
      .references("id")
      .inTable("tags")
      .onDelete("CASCADE");
  });

  // Create 'time_blocks' table
  await knex.schema.createTable("time_blocks", (table) => {
    table.increments("id").primary();
    table
      .integer("main_tag_id")
      .unsigned()
      .references("id")
      .inTable("tags")
      .notNullable();
    table.timestamp("start_time").notNullable();
    table.timestamp("end_time").notNullable();
    table.text("text").notNullable();
  });
};

exports.down = async function (knex) {
  await knex.schema.dropTableIfExists("time_blocks");
  await knex.schema.dropTableIfExists("tags_to_things");
  await knex.schema.dropTableIfExists("tags");
  await knex.schema.dropTableIfExists("things");
};
