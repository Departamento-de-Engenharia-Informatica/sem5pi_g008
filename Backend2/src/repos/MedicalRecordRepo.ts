import {Inject, Service} from "typedi";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";
import {Document, Model} from "mongoose";
import {IMedicalRecordPersistence} from "../dataschema/IMedicalRecordPersistence";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";


@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo{
    constructor(@Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,) {
    }

    exists(t: MedicalRecord): Promise<boolean> {
        return Promise.resolve(false);
    }

    save(t: MedicalRecord): Promise<MedicalRecord> {
        return Promise.resolve(undefined);
    }


    
    
}