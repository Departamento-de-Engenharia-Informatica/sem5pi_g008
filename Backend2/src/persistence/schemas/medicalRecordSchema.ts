import mongoose from 'mongoose';
import {IMedicalRecordPersistence} from "../../dataschema/IMedicalRecordPersistence";
import AllergySchema from "./allergySchema";
import MedicalConditionSchema from "./medicalConditionSchema";


const MedicalRecordSchema= new mongoose.Schema(
    {
        domainId:{ type: Number, unique:true},
        allergies:{type: [AllergySchema], default:[]},
        medicalCondition:{type: [MedicalConditionSchema], default:[]},
        freeText:{type: String}
    },
    { 
        timestamps: true
    }
);

export default mongoose.model<IMedicalRecordPersistence & mongoose.Document>('MedicalRecord', MedicalRecordSchema);