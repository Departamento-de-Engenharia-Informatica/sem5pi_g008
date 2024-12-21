import mongoose from 'mongoose';
import {IAllergyPersistence} from "../../dataschema/IAllergyPersistence";

const AllergySchema = new mongoose.Schema(
  {
      domainId: { type: Number, unique: true },
      allergy: { type: String, unique: true },
      effect: { type: String },
      isDeleted: { type: Boolean, default: false }
  },
  {
      timestamps: true
  }
);

export default mongoose.model<IAllergyPersistence & mongoose.Document>('Allergy', AllergySchema);
