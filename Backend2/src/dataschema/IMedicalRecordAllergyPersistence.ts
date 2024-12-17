import mongoose from "mongoose";

export interface IMedicalRecordAllergyPersistence {
  domainId: number;
  allergyId: mongoose.Types.ObjectId;
  medicalRecordId: mongoose.Types.ObjectId;
  doctorId: string;
  comment: string;
}
