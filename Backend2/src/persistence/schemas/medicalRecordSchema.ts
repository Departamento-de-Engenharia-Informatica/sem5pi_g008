import mongoose from 'mongoose';
import { IMedicalRecordPersistence } from "../../dataschema/IMedicalRecordPersistence";
import AllergySchema from "./allergySchema";
import MedicalConditionSchema from "./medicalConditionSchema";

const MedicalRecordSchema = new mongoose.Schema(
  {
    domainId: { type: Number, unique: true, required: true }, // Adding required for domainId
    allergies: [{
      type: mongoose.Schema.Types.ObjectId,
      ref: 'Allergy',
      required: false
    }],
    medicalCondition: [{
      type: mongoose.Schema.Types.ObjectId,
      ref: 'MedicalCondition',
      required: false
    }],
    freeText: [{
      type: String,
      required: false
    }]
  },
  {
    timestamps: true // Adds createdAt and updatedAt fields
  }
);

export default mongoose.model<IMedicalRecordPersistence & mongoose.Document>('MedicalRecord', MedicalRecordSchema);
