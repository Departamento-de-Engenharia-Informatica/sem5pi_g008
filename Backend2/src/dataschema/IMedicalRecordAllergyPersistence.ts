import mongoose from "mongoose";

export interface IMedicalRecordAllergyPersistence {
  allergyId: mongoose.Types.ObjectId;
  doctorId: string;
  comment: string;
}
