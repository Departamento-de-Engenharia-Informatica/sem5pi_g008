import mongoose from 'mongoose';
import { IMedicalRecordFamilyHistoryPersistence } from "../../dataschema/IMedicalRecordFamilyHistoryPersistence";

let Schema = mongoose.Schema;

// Defining the schema for MedicalRecordFamilyHistory
const MedicalRecordFamilyHistorySchema = new mongoose.Schema(
    {
        domainId: { type: Number, unique: true, required: true }, // Unique identifier
        medicalRecordId: { type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: true }, // Reference to the medical record
        familyMember: { type: String, required: true }, // Family member (e.g., father, mother, etc.)
        condition: { type: String, required: true }, // Medical condition
        ageOfDiagnosis: { type: Number, required: false }, // Age of diagnosis, optional
        isChronic: { type: Boolean, required: true }, // Chronic condition flag
        additionalDetails: { type: String, required: false } // Optional additional details or comments
    },
    {
        timestamps: true // Add createdAt and updatedAt timestamps
    }
);

// Export the schema as a model
export default mongoose.model<IMedicalRecordFamilyHistoryPersistence & mongoose.Document>('MedicalRecordFamilyHistory', MedicalRecordFamilyHistorySchema);
