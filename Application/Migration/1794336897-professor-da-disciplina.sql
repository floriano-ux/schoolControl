ALTER TABLE subjects ADD COLUMN professor_id UUID DEFAULT NULL;
CREATE INDEX subjects_professor_id_index ON subjects (professor_id);
ALTER TABLE subjects ADD CONSTRAINT subjects_ref_professor_id FOREIGN KEY (professor_id) REFERENCES users (id) ON DELETE SET NULL;
