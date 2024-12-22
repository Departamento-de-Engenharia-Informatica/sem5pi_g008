import {Service, Inject} from 'typedi';
import IMedicalConditionRepo from "../services/IRepos/IMedicalConditionRepo";
import mongoose, {Document, Model} from "mongoose";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import {MedicalConditionId} from "../domain/MedicalCondition/MedicalConditionId";
import {Designation} from "../domain/MedicalCondition/designation";
import {Description} from "../domain/MedicalCondition/description";
import {Code} from "../domain/MedicalCondition/code";
import {cloneWith} from "lodash";

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {

    constructor(
        @Inject('medicalConditionSchema') private medicalConditionSchema: Model<IMedicalConditionPersistence & Document>,
    ) {
    }

    public async save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition> {

        if(id === undefined) {
            id = await this.getLastId() + 1;
        }

        const rawMedicalCondition: any = MedicalConditionMap.toPersistence(medicalCondition, id);

        const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

        return MedicalConditionMap.toDomain(medicalConditionCreated);
    }

    public async getLastId(): Promise<number> {
        const lastElement = await this.medicalConditionSchema.find().sort({domainId: -1}).limit(1);

        if (lastElement.length === 0) {
            return 0;
        }

        return lastElement[0].domainId;
    }

    public async getOne(): Promise<MedicalCondition> {
        const medicalCondition = await this.medicalConditionSchema.findOne();

        return MedicalConditionMap.toDomain(medicalCondition);
    }


    public async exists(medicalConditionId: MedicalConditionId | string): Promise<boolean> {

        const idX = medicalConditionId instanceof MedicalConditionId ? (<MedicalConditionId>medicalConditionId).id.toValue() : medicalConditionId;

        const query = {domainId: idX};
        const userDocument = await this.medicalConditionSchema.findOne(query);

        return !!userDocument === true;
    }
    
    public async getMedicalConditionByCode(code: Code): Promise<MedicalCondition> {
        const query = {code: code.value};
        const medicalCondition = await this.medicalConditionSchema.findOne(query);

        return MedicalConditionMap.toDomain(medicalCondition);
    } 
    public async searchDesignation(query: string): Promise<MedicalCondition[]> { 
        try {
            const conditions = await this.medicalConditionSchema.find(
                    { designation: query}).exec();
            console.log("Conditions: ", conditions);
            

            if (!conditions || conditions.length === 0) {
                return [];
            } 

            return conditions.map((condition) => MedicalConditionMap.toDomain(condition));
        } catch (error) {
            console.error("Error while searching for medical conditions:", error);
            throw new Error("Failed to search medical conditions.");
        } 
    }
    public async searchCode(query: string): Promise<MedicalCondition[]> {
        try {
            const conditions = await this.medicalConditionSchema.find(
                { code: query}).exec();
            console.log("Conditions: ", conditions);


            if (!conditions || conditions.length === 0) {
                return [];
            }

            return conditions.map((condition) => MedicalConditionMap.toDomain(condition));
        } catch (error) {
            console.error("Error while searching for medical conditions:", error);
            throw new Error("Failed to search medical conditions.");
        }
    }

    public async getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any> {

        const objectId = new mongoose.Types.ObjectId(medicalConditionId);

        const medicalCondition = await this.medicalConditionSchema.findOne( { _id: objectId }).exec();

        return MedicalConditionMap.toDomain(medicalCondition);
    }

    public async getAll(): Promise<MedicalCondition[]> {
        const medicalConditions = await this.medicalConditionSchema.find();
        return medicalConditions.map((medicalCondition) => MedicalConditionMap.toDomain(medicalCondition));
    }

}
