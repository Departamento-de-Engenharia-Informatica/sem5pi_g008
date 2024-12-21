import {Service, Inject} from 'typedi';
import IMedicalConditionRepo from "../services/IRepos/IMedicalConditionRepo";
import {Document, Model} from "mongoose";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import {MedicalConditionId} from "../domain/MedicalCondition/MedicalConditionId";

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

}