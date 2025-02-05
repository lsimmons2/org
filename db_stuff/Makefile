PG_USER=leo
PROD_DB=org_prod
DEV_DB=org_dev

TIMESTAMP=$(shell date +%Y%m%d_%H%M%S)

PROD_DUMP=db_dumps/prod_backup_$(TIMESTAMP).sql
DEV_DUMP=db_dumps/dev_backup_$(TIMESTAMP).sql

DUMP_FLAGS=--clean --if-exists --format=custom -p 5433
RESTORE_FLAGS=--clean --if-exists -p 5433

# Directories to sync
PROD_SET_DIR=/Users/leo/dev/org/set_db/prod/
DEV_SET_DIR=/Users/leo/dev/org/set_db/dev/

.PHONY: prod-to-dev dev-to-prod dump-prod dump-dev restore-prod restore-dev clean

dump-prod:
	pg_dump -U $(PG_USER) $(DUMP_FLAGS) -d $(PROD_DB) -f $(PROD_DUMP)
	echo "Production database dumped to $(PROD_DUMP)"

prod-to-dev: dump-prod
	rsync -av --delete $(PROD_SET_DIR) $(DEV_SET_DIR)
	echo "Synced $(PROD_SET_DIR) → $(DEV_SET_DIR)"
	pg_restore -U $(PG_USER) $(RESTORE_FLAGS) -d $(DEV_DB) $(PROD_DUMP)
	echo "Production data copied to development database"

dump-dev:
	pg_dump -U $(PG_USER) $(DUMP_FLAGS) -d $(DEV_DB) -f $(DEV_DUMP)
	echo "Development database dumped to $(DEV_DUMP)"

dev-to-prod: dump-dev
	rsync -av --delete $(DEV_SET_DIR) $(PROD_SET_DIR)
	echo "Synced $(DEV_SET_DIR) → $(PROD_SET_DIR)"
	pg_restore -U $(PG_USER) $(RESTORE_FLAGS) -d $(PROD_DB) $(DEV_DUMP)
	echo "Development data copied to production database"

clean:
	find . -name "prod_backup_*.sql" -o -name "dev_backup_*.sql" -type f -delete
	echo "Old dump files removed"
