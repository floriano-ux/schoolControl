-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE enrollment_status AS ENUM ('ativa', 'trancada', 'concluida');
CREATE TYPE user_role AS ENUM ('admin', 'coordenador', 'professor', 'aluno');

CREATE TABLE courses (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    workload INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE subjects (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    course_id UUID NOT NULL,
    name TEXT NOT NULL,
    workload INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    professor_id UUID DEFAULT NULL
);

CREATE TABLE enrollments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    student_id UUID NOT NULL,
    subject_id UUID NOT NULL,
    status enrollment_status DEFAULT 'ativa' NOT NULL
);

-- AVn = 70% PE(prova escrita) + 30% AVf(avaliação formativa).
-- pe3/avf3 são opcionais: a AV3 só é lançada se a média de AV1 e AV2 for >= 4,0.
CREATE TABLE grades (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    enrollment_id UUID NOT NULL,
    pe1 DOUBLE PRECISION NOT NULL,
    avf1 DOUBLE PRECISION NOT NULL,
    pe2 DOUBLE PRECISION NOT NULL,
    avf2 DOUBLE PRECISION NOT NULL,
    pe3 DOUBLE PRECISION DEFAULT NULL,
    avf3 DOUBLE PRECISION DEFAULT NULL
);

-- Usuários do sistema (login do IHP). locked_at e failed_login_attempts são exigidos
-- pelo IHP: após 10 tentativas erradas a conta fica bloqueada por 1 hora.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    user_role user_role DEFAULT 'aluno' NOT NULL,
    document TEXT DEFAULT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE INDEX enrollments_student_id_index ON enrollments (student_id);
CREATE INDEX enrollments_subject_id_index ON enrollments (subject_id);
CREATE INDEX subjects_course_id_index ON subjects (course_id);
CREATE INDEX subjects_professor_id_index ON subjects (professor_id);
CREATE INDEX grades_enrollment_id_index ON grades (enrollment_id);

ALTER TABLE enrollments ADD CONSTRAINT enrollments_ref_student_id FOREIGN KEY (student_id) REFERENCES users (id) ON DELETE CASCADE;
ALTER TABLE enrollments ADD CONSTRAINT enrollments_ref_subject_id FOREIGN KEY (subject_id) REFERENCES subjects (id) ON DELETE CASCADE;
ALTER TABLE subjects ADD CONSTRAINT subjects_ref_course_id FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE;
ALTER TABLE subjects ADD CONSTRAINT subjects_ref_professor_id FOREIGN KEY (professor_id) REFERENCES users (id) ON DELETE SET NULL;
ALTER TABLE grades ADD CONSTRAINT grades_ref_enrollment_id FOREIGN KEY (enrollment_id) REFERENCES enrollments (id) ON DELETE CASCADE;

-- Restrições de integridade (Sprint 2)
ALTER TABLE users ADD CONSTRAINT users_email_key UNIQUE (email);
ALTER TABLE courses ADD CONSTRAINT courses_workload_check CHECK (workload > 0);
ALTER TABLE subjects ADD CONSTRAINT subjects_workload_check CHECK (workload > 0);
ALTER TABLE subjects ADD CONSTRAINT subjects_course_id_name_key UNIQUE (course_id, name);
ALTER TABLE enrollments ADD CONSTRAINT enrollments_student_id_subject_id_key UNIQUE (student_id, subject_id);
ALTER TABLE grades ADD CONSTRAINT grades_pe1_check CHECK ((pe1 >= 0) AND (pe1 <= 10));
ALTER TABLE grades ADD CONSTRAINT grades_avf1_check CHECK ((avf1 >= 0) AND (avf1 <= 10));
ALTER TABLE grades ADD CONSTRAINT grades_pe2_check CHECK ((pe2 >= 0) AND (pe2 <= 10));
ALTER TABLE grades ADD CONSTRAINT grades_avf2_check CHECK ((avf2 >= 0) AND (avf2 <= 10));
ALTER TABLE grades ADD CONSTRAINT grades_pe3_check CHECK ((pe3 >= 0) AND (pe3 <= 10));
ALTER TABLE grades ADD CONSTRAINT grades_avf3_check CHECK ((avf3 >= 0) AND (avf3 <= 10));
