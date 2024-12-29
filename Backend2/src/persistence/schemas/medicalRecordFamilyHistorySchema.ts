import mongoose from 'mongoose';
import { IMedicalRecordFamilyHistoryPersistence } from "../../dataschema/IMedicalRecordFamilyHistoryPersistence";

let Schema = mongoose.Schema;

// Defining the schema for MedicalRecordFamilyHistory
const MedicalRecordFamilyHistorySchema = new mongoose.Schema(
    {
        domainId: { type: Number, unique: true, required: true }, // Unique identifier
        medicalRecordId: { type: Schema.Types.ObjectId, ref: 'MedicalRecord', required: false }, // Reference to the medical record
        familyMember: { type: String, required: true }, // Family member (e.g., father, mother, etc.)
        condition: { type: String, required: true }, // Medical condition
    }, 
    {
        timestamps: true // Add createdAt and updatedAt timestamps
    }
);

// Export the schema as a model
export default mongoose.model<IMedicalRecordFamilyHistoryPersistence & mongoose.Document>('MedicalRecordFamilyHistory', MedicalRecordFamilyHistorySchema);
