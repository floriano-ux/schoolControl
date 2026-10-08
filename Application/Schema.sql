-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.

CREATE TABLE students (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE courses (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    name TEXT NOT NULL,
    workload INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE enrollments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    student_id UUID NOT NULL,
    course_id UUID NOT NULL,
    status TEXT NOT NULL,
    CONSTRAINT fk_student FOREIGN KEY (student_id) REFERENCES students (id) ON DELETE CASCADE,
    CONSTRAINT fk_course FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE
);

CREATE TABLE grades (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    enrollment_id UUID NOT NULL,
    p1 DOUBLE PRECISION NOT NULL,
    p2 DOUBLE PRECISION NOT NULL,
    final_exam DOUBLE PRECISION,
    CONSTRAINT fk_enrollment FOREIGN KEY (enrollment_id) REFERENCES enrollments (id) ON DELETE CASCADE
);